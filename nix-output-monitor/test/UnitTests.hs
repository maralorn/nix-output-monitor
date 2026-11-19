import Data.ByteString.Char8 qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as Seq
import Data.Set (singleton)
import Data.Strict qualified as Strict
import Data.Text qualified as Text
import Data.Time (getZonedTime)
import NOM.Builds
import NOM.NixMessage.OldStyle (NixOldStyleMessage (..))
import NOM.Parser
import NOM.Print (Config (..), stateToText)
import NOM.State qualified as State
import NOM.State.CacheId (CacheId (..))
import NOM.State.CacheId.Map qualified as CMap
import NOM.StreamParser (stripANSICodes)
import NOM.Util (parseOne)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Console.Terminal.Size (Window (..))
import Test.HUnit

assertOldStyleParse :: ByteString -> IO (ByteString, NixOldStyleMessage)
assertOldStyleParse input = do
  let res = parseOne parser input
  assertBool "parsing succeeds" (isJust res)
  let (t, res') = Unsafe.fromJust res
  assertBool "parsing succeeds with an actual match" (isJust res')
  pure (t, Unsafe.fromJust res')

failedDerivations :: [(CacheId Derivation, Derivation, State.BuildInfo State.BuildFail, State.DerivationInfo)]
failedDerivations =
  zipWith
    mkFailedDerivation
    [0 ..]
    [ StorePath "00000000000000000000000000000000" "example-one"
    , StorePath "11111111111111111111111111111111" "example-two"
    , StorePath "22222222222222222222222222222222" "example-three"
    ]
 where
  mkFailedDerivation key storePath =
    let drv = Derivation storePath
        buildInfo =
          State.MkBuildInfo
            0
            Localhost
            Strict.Nothing
            Strict.Nothing
            (State.MkBuildFail (fromIntegral key) (ExitCode 1))
        drvInfo =
          State.MkDerivationInfo
            drv
            mempty
            mempty
            mempty
            (State.Failed buildInfo)
            mempty
            False
            mempty
            Strict.Nothing
            Strict.Nothing
     in (MkCacheId key, drv, buildInfo, drvInfo)

emptyState :: Seq Text -> State.NOMState
emptyState errors =
  State.MkNOMState
    mempty
    mempty
    mempty
    mempty
    mempty
    0
    State.Finished
    mempty
    mempty
    mempty
    mempty
    errors
    mempty
    Strict.Nothing
    mempty
    (State.MkEvalInfo Strict.Nothing 0 0)

failureState :: [Text] -> State.NOMState
failureState additionalErrors =
  (emptyState $ Seq.fromList $ directFailureErrors <> dependencyErrors <> additionalErrors)
    { State.derivationInfos = derivationInfos
    , State.fullSummary = dependencySummary
    , State.forestRoots = Seq.fromList $ failedDerivations <&> viewFailureId
    , State.derivationIds = Map.fromList $ failedDerivations <&> \(drvId, drv, _, _) -> (drv, drvId)
    }
 where
  viewFailureId (drvId, _, _, _) = drvId
  derivationInfos =
    foldl'
      (\infos (drvId, _, _, drvInfo) -> CMap.insert drvId drvInfo infos)
      mempty
      failedDerivations
  failedBuilds =
    foldl'
      (\builds (drvId, _, buildInfo, _) -> CMap.insert drvId buildInfo builds)
      mempty
      failedDerivations
  dependencySummary =
    State.MkDependencySummary
      mempty
      mempty
      mempty
      failedBuilds
      mempty
      mempty
      mempty
      mempty
      mempty
  directFailureErrors = failedDerivations <&> \(_, drv, _, _) -> directFailureError drv
  directFailureError drv =
    Text.unlines
      [ "error: Cannot build '" <> toText drv <> "'."
      , "       Reason: builder failed with exit code 1."
      , "       Last 25 log lines:"
      , "       > detailed build output"
      ]
  dependencyErrors =
    [ Text.unlines
        [ "error: Cannot build '/nix/store/00000000000000000000000000000000-dependent-" <> show index <> ".drv'."
        , "       Reason: 1 dependency failed."
        ]
    | index <- [1 .. 15 :: Int]
    ]

renderState :: State.NOMState -> IO Text
renderState nomState = do
  now <- getZonedTime
  pure
    $ stripANSICodes
    $ stateToText
      MkConfig{silent = False, piping = False}
      nomState
      (Just $ Window 30 200)
      (now, 10)

assertFailureRendering :: IO ()
assertFailureRendering = do
  rendered <- renderState $ failureState []
  assertBool "direct failures have their own heading" ("3 Failed Builds:" `Text.isInfixOf` rendered)
  let afterGraph = snd $ Text.breakOn "Dependency Graph" rendered
  forM_ failedDerivations \(_, drv, _, _) -> do
    assertBool ("failed derivation is visible: " <> toString (toText drv)) (toText drv `Text.isInfixOf` rendered)
    assertBool ("failed derivation follows the graph: " <> toString (toText drv)) (toText drv `Text.isInfixOf` afterGraph)
  assertBool "dependent failures are summarized" ("15 dependent failures omitted" `Text.isInfixOf` rendered)
  assertBool "raw build errors are omitted" (not $ "18 Errors:" `Text.isInfixOf` rendered)
  assertBool "build log tails are omitted" (not $ "Last 25 log lines:" `Text.isInfixOf` rendered)
  assertBool "the rendered status fits the viewport" (length (Text.lines rendered) <= 30)

assertUnrelatedErrorRendering :: IO ()
assertUnrelatedErrorRendering = do
  let unrelatedError = "error: undefined variable 'foo'"
  rendered <- renderState $ failureState [unrelatedError]
  assertBool "unrelated errors retain their own heading" ("1 Errors:" `Text.isInfixOf` rendered)
  assertBool "unrelated errors remain visible" (unrelatedError `Text.isInfixOf` rendered)

assertUnrecognizedDependencyErrorRendering :: IO ()
assertUnrecognizedDependencyErrorRendering = do
  let dependencyError =
        Text.unlines
          [ "error: Cannot build '/nix/store/00000000000000000000000000000000-dependent.drv'."
          , "       Reason: 1 dependency failed."
          ]
  rendered <- renderState $ emptyState $ Seq.fromList [dependencyError]
  assertBool "dependency derivation remains visible without a canonical failure" ("dependent.drv" `Text.isInfixOf` rendered)
  assertBool "dependency reason remains visible without a canonical failure" ("Reason: 1 dependency failed." `Text.isInfixOf` rendered)

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
        , "Render failed derivations instead of build logs" ~: assertFailureRendering
        , "Preserve unrelated errors alongside failed derivations" ~: assertUnrelatedErrorRendering
        , "Preserve dependency errors without a recognized failed derivation" ~: assertUnrecognizedDependencyErrorRendering
        ]
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
