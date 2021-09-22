module Main (main) where

import PrettyPrintGoldenTest
import Test.Tasty

main :: IO ()
main = do
  goldens <- prettyPrintGoldentTest
  defaultMain
    ( testGroup
        "All tests"
        [ testGroup "Golden tests" goldens
        ]
    )
