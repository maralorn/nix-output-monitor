import Control.Exception (bracket)
import Data.ByteString.Char8 qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Set (singleton)
import Data.Time (UTCTime (..), fromGregorian)
import NOM.Builds
import NOM.NixMessage.OldStyle (NixOldStyleMessage (..))
import NOM.Parser
import NOM.Update.Monad.CacheBuildReports (BuildReportKey (..), MonadCacheBuildReports (..))
import NOM.Util (parseOne)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Directory (createDirectory, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.HUnit

assertOldStyleParse :: ByteString -> IO (ByteString, NixOldStyleMessage)
assertOldStyleParse input = do
  let res = parseOne parser input
  assertBool "parsing succeeds" (isJust res)
  let (t, res') = Unsafe.fromJust res
  assertBool "parsing succeeds with an actual match" (isJust res')
  pure (t, Unsafe.fromJust res')

withBuildReportsFile :: (FilePath -> IO a) -> IO a
withBuildReportsFile action = bracket acquire removePathForcibly \dir ->
  bracket (lookupEnv "XDG_STATE_HOME") restore \_ -> do
    setEnv "XDG_STATE_HOME" dir
    let reportsDir = dir </> "nix-output-monitor"
    createDirectory reportsDir
    action (reportsDir </> "build-reports.csv")
 where
  acquire = do
    tmp <- getTemporaryDirectory
    (path, handle) <- openTempFile tmp "nom-build-reports"
    hClose handle
    removeFile path
    createDirectory path
    pure path
  restore = maybe (unsetEnv "XDG_STATE_HOME") (setEnv "XDG_STATE_HOME")

buildReportTests :: Test
buildReportTests =
  "Build report cache"
    ~: test
      [ "Legacy CSV" ~: withBuildReportsFile \path -> do
          writeFileBS path "hostname,derivation name,utc time,build seconds\n,example,2026-01-01 00:00:00,12\n"
          reports <- getCachedBuildReports
          assertEqual
            "missing platform columns remain unknown"
            (Map.singleton (BuildReportKey Localhost "example" Nothing Nothing) (Map.singleton timestamp 12))
            reports
      , "Blank platform columns" ~: withBuildReportsFile \path -> do
          writeFileBS path "hostname,derivation name,build platform,target platform,utc time,build seconds\nbuilder,example,,,2026-01-01 00:00:00,12\n"
          reports <- getCachedBuildReports
          assertEqual
            "empty platform fields remain unknown"
            (Map.singleton (BuildReportKey (Hostname "builder") "example" Nothing Nothing) (Map.singleton timestamp 12))
            reports
      , "Platform histories survive saving and loading" ~: withBuildReportsFile \_ -> do
          let key = BuildReportKey Localhost "example" (Just "x86_64-linux") (Just "armv7l-linux")
              keys =
                [ key
                , key{targetPlatform = Just "i686-linux"}
                , key{buildPlatform = Just "aarch64-linux"}
                , key{host = Hostname "builder", buildPlatform = Nothing}
                , key{drvName = "another,package"}
                , key{buildPlatform = Nothing, targetPlatform = Nothing}
                ]
              expected = Map.fromList $ zipWith (\k duration -> (k, Map.fromList [(timestamp, duration), (later, duration + 1)])) keys [10 ..]
          _ <- updateBuildReports (const expected)
          reports <- getCachedBuildReports
          assertEqual "distinct hosts, names and platforms retain their own histories" expected reports
      ]
 where
  timestamp = UTCTime (fromGregorian 2026 1 1) 0
  later = UTCTime (fromGregorian 2026 1 2) 0

main :: IO ()
main = do
  counts <-
    runTestTT
      $ test
        [ buildReportTests
        , "Parse Plan" ~: do
            (rest, result) <-
              assertOldStyleParse
                "these derivations will be built:\n  /nix/store/7n05q79qhrgvnfmvv2v3cnj3yqf4d1hf-haskell-language-server-0.4.0.0.drv\nthese paths will be fetched (134.19 MiB download, 1863.82 MiB unpacked):\n  /nix/store/60zb5dndaw1fzir3s69sy3xhy19gll1p-ghc-8.8.2\ngarbage"
            assertEqual
              "result matches"
              ( PlanBuilds
                  ( singleton
                      ( Derivation
                          $ StorePath
                            "7n05q79qhrgvnfmvv2v3cnj3yqf4d1hf"
                            "haskell-language-server-0.4.0.0"
                      )
                  )
                  ( Derivation
                      $ StorePath
                        "7n05q79qhrgvnfmvv2v3cnj3yqf4d1hf"
                        "haskell-language-server-0.4.0.0"
                  )
              )
              result
            (rest2, result2) <- assertOldStyleParse rest
            assertEqual
              "result matches"
              ( PlanDownloads
                  (134.19 * 1024 ** 2)
                  (1863.82 * 1024 ** 2)
                  (singleton (StorePath "60zb5dndaw1fzir3s69sy3xhy19gll1p" "ghc-8.8.2"))
              )
              result2
            assertEqual "rest is okay" "garbage" rest2
        , "Parse Downloading" ~: do
            (rest, result) <-
              assertOldStyleParse
                "copying path '/nix/store/yk1164s4bkj6p3s4mzxm5fc4qn38cnmf-ghc-8.8.2-doc' from 'https://cache.nixos.org'...\n"
            assertEqual
              "result matches"
              ( Downloading
                  (StorePath "yk1164s4bkj6p3s4mzxm5fc4qn38cnmf" "ghc-8.8.2-doc")
                  (Host (Just "https") Nothing "cache.nixos.org")
              )
              result
            assertEqual "no rest" "" rest
        , "Parse local building" ~: do
            (rest, result) <-
              assertOldStyleParse
                "building '/nix/store/dpqlnrbvzhjxp06d1mc3ksf2w8m2ldms-aeson-1.5.2.0.drv'...\n"
            assertEqual
              "result matches"
              ( Build
                  (Derivation $ StorePath "dpqlnrbvzhjxp06d1mc3ksf2w8m2ldms" "aeson-1.5.2.0")
                  Localhost
              )
              result
            assertEqual "no rest" "" rest
        , "Parse remote building" ~: do
            (rest, result) <-
              assertOldStyleParse
                "building '/nix/store/63jjdifv1x1nymjxdwla603xy1sggakk-hoogle-local-0.1.drv' on 'ssh://maralorn@example.com'...\n"
            assertEqual
              "result matches"
              ( Build
                  (Derivation $ StorePath "63jjdifv1x1nymjxdwla603xy1sggakk" "hoogle-local-0.1")
                  (Host (Just "ssh") (Just "maralorn") "example.com")
              )
              result
            assertEqual "no rest" "" rest
        , "Parse failed build" ~: do
            (rest, result) <-
              assertOldStyleParse
                "builder for '/nix/store/fbpdwqrfwr18nn504kb5jqx7s06l1mar-regex-base-0.94.0.1.drv' failed with exit code 1\n"
            assertEqual
              "result matches"
              (Failed (Derivation $ StorePath "fbpdwqrfwr18nn504kb5jqx7s06l1mar" "regex-base-0.94.0.1") (ExitCode 1))
              result
            assertEqual "no rest" "" rest
        , "Parse failed build for nix 2.4" ~: do
            (rest, result) <-
              assertOldStyleParse
                "error: builder for '/nix/store/dylih0mw8yisn6nrjc3qlf51knmdkrq1-local-build-3.drv' failed with exit code 1;\n"
            assertEqual
              "result matches"
              (Failed (Derivation $ StorePath "dylih0mw8yisn6nrjc3qlf51knmdkrq1" "local-build-3") (ExitCode 1))
              result
            assertEqual "no rest" "" rest
        , "Parse failed build for nix >= 2.29" ~: do
            (_, result) <-
              assertOldStyleParse
                $ ByteString.unlines
                  [ "error: Cannot build '/nix/store/d055cqki6z1vll144kvj496cknwvwi44-build-fail.drv'."
                  , "       Reason: builder failed with exit code 1."
                  , "       Output paths:"
                  , "          /nix/store/pvf324ikpfb9nhyszmc4zz5g9y8by0f6-build-fail"
                  ]
            assertEqual
              "result matches"
              (Failed (Derivation $ StorePath "d055cqki6z1vll144kvj496cknwvwi44" "build-fail") (ExitCode 1))
              result
        ]
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
