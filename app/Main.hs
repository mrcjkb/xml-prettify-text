module Main (main) where

import Prelude
import Text.XML.Prettify
import qualified Data.Text.IO as TIO
import Control.DeepSeq


main :: IO ()
main = profileSimulate

-- Simulates repeated uses for profiling
profileSimulate :: IO ()
profileSimulate = do
  uglyXml <- TIO.readFile uglyXmlFile
  let uglyXmlList = replicate nRuns uglyXml
  let prettyXmlList = map prettyPrintXml uglyXmlList
  pure (deepseq prettyXmlList "Done") >>= putStrLn
  where
    uglyXmlFile :: FilePath
    uglyXmlFile = "data/tests/prettyPrintTest.xml"
    nRuns = 10000

