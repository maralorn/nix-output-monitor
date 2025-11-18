import Data.Set (singleton)
import NOM.Builds
import NOM.NixMessage.OldStyle (NixOldStyleMessage (..))
import NOM.Parser
import NOM.Util (parseOne)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.HUnit

assertOldStyleParse :: ByteString -> IO (ByteString, NixOldStyleMessage)
assertOldStyleParse input = do
  let res = parseOne parser input
  assertBool "parsing succeeds" (isJust res)
  let (t, res') = Unsafe.fromJust res
  assertBool "parsing succeeds with an actual match" (isJust res')
  pure (t, Unsafe.fromJust res')

main :: IO ()
main = do
  counts <-
    runTestTT
      $ test
        [ "Parse Plan" ~: do
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
        , "Parse faild build for nix 2.4" ~: do
            (rest, result) <-
              assertOldStyleParse
                "error: builder for '/nix/store/dylih0mw8yisn6nrjc3qlf51knmdkrq1-local-build-3.drv' failed with exit code 1;\n"
            assertEqual
              "result matches"
              (Failed (Derivation $ StorePath "dylih0mw8yisn6nrjc3qlf51knmdkrq1" "local-build-3") (ExitCode 1))
              result
            assertEqual "no rest" "" rest
        ]
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
