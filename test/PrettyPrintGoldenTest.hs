{-# LANGUAGE OverloadedStrings #-}

module PrettyPrintGoldenTest
  ( prettyPrintGoldentTest,
  )
where

import qualified Data.Text.IO as TIO
import System.FilePath (normalise, replaceExtension, takeBaseName)
import Test.Tasty
import Test.Tasty.Golden
import Text.XML.Prettify

prettyPrintGoldentTest :: IO [TestTree]
prettyPrintGoldentTest = sequence [goldenPrettyPrint]

goldenPrettyPrint :: IO TestTree
goldenPrettyPrint = testGroup "Pretty print XML" . map createTest
    <$> findByExtension [".xml"] testsDir
      where
        testsDir = normalise "data/tests"


createTest :: [Char] -> TestTree
createTest testFile =
  goldenVsFile
    (takeBaseName testFile)
    goldenf
    outf
    testAction
  where
    goldenf = replaceExtension testFile ".out.golden"
    outf = replaceExtension testFile ".out"
    testAction = do
      prettyPrintedXml <- prettyPrintXml <$> TIO.readFile testFile
      TIO.writeFile outf prettyPrintedXml
