import Relude
import Prelude ()

import Data.Attoparsec.Text (IResult (Done), Parser, parse)
import Data.Maybe (fromJust)
import Data.Set (singleton)
import Test.HUnit

import NOM.Parser

assertParse :: Parser (a, Text) -> Text -> IO (Text, a)
assertParse parser' input = do
  let res = p $ parse parser' input
  assertBool "parsing succeeds" (isJust res)
  pure (fromJust res)
 where
  p (Done x (a, _)) = Just (x, a)
  p _ = Nothing

main :: IO ()
main = do
  counts <-
    runTestTT $
      test
        [ "Parse Plan" ~: do
            (rest, result) <-
              assertParse
                parser
                "these derivations will be built:\n  /nix/store/7n05q79qhrgvnfmvv2v3cnj3yqf4d1hf-haskell-language-server-0.4.0.0.drv\nthese paths will be fetched (134.19 MiB download, 1863.82 MiB unpacked):\n  /nix/store/60zb5dndaw1fzir3s69sy3xhy19gll1p-ghc-8.8.2\ngarbage"
            assertEqual
              "result matches"
              ( PlanBuilds
                  ( singleton
                      ( Derivation $
                          StorePath
                            "7n05q79qhrgvnfmvv2v3cnj3yqf4d1hf"
                            "haskell-language-server-0.4.0.0"
                      )
                  )
                  ( Derivation $
                      StorePath
                        "7n05q79qhrgvnfmvv2v3cnj3yqf4d1hf"
                        "haskell-language-server-0.4.0.0"
                  )
              )
              result
            (rest2, result2) <- assertParse parser rest
            assertEqual
              "result matches"
              ( PlanDownloads
                  134.19
                  1863.82
                  (singleton (StorePath "60zb5dndaw1fzir3s69sy3xhy19gll1p" "ghc-8.8.2"))
              )
              result2
            assertEqual "rest is okay" "garbage" rest2
        , "Parse Downloading" ~: do
            (rest, result) <-
              assertParse
                parser
                "copying path '/nix/store/yk1164s4bkj6p3s4mzxm5fc4qn38cnmf-ghc-8.8.2-doc' from 'https://cache.nixos.org'...\n"
            assertEqual
              "result matches"
              ( Downloading
                  (StorePath "yk1164s4bkj6p3s4mzxm5fc4qn38cnmf" "ghc-8.8.2-doc")
                  (Host "https://cache.nixos.org")
              )
              result
            assertEqual "no rest" "" rest
        , "Parse local building" ~: do
            (rest, result) <-
              assertParse
                parser
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
              assertParse
                parser
                "building '/nix/store/63jjdifv1x1nymjxdwla603xy1sggakk-hoogle-local-0.1.drv' on 'ssh://maralorn@example.com'...\n"
            assertEqual
              "result matches"
              ( Build
                  (Derivation $ StorePath "63jjdifv1x1nymjxdwla603xy1sggakk" "hoogle-local-0.1")
                  (Host "ssh://maralorn@example.com")
              )
              result
            assertEqual "no rest" "" rest
        , "Parse failed build" ~: do
            (rest, result) <-
              assertParse
                parser
                "builder for '/nix/store/fbpdwqrfwr18nn504kb5jqx7s06l1mar-regex-base-0.94.0.1.drv' failed with exit code 1\n"
            assertEqual
              "result matches"
              (Failed (Derivation $ StorePath "fbpdwqrfwr18nn504kb5jqx7s06l1mar" "regex-base-0.94.0.1") 1)
              result
            assertEqual "no rest" "" rest
        ]
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
