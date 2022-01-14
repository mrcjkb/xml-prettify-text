{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import qualified Data.Text.IO as TIO
import Options.Applicative as Opt
import System.IO.Error (ioeGetFileName, isDoesNotExistError)
import Text.XML.Prettify
import Prelude
import GHC.IO.Exception (ExitCode)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main =
  (execParser options >>= runMainWithConfig)
    `catches` [ Handler parserExit,
                Handler printIOError,
                Handler printError
              ]
  where
    options =
      info
        (mkOptions <**> helper)
        ( fullDesc
            <> progDesc
              "Pretty-prints XML text"
        )
    parserExit :: ExitCode -> IO ()
    parserExit _ = pure ()
    printIOError :: IOException -> IO ()
    printIOError e
      | isDoesNotExistError e = do
        let mbfn = ioeGetFileName e
        putStrLn $ "File " ++ fromMaybe "" mbfn ++ " not found."
      | otherwise = putStrLn $ "I/O error: " ++ show e
    printError :: SomeException -> IO ()
    printError = print

runMainWithConfig :: Options -> IO ()
runMainWithConfig Options {..} = do
  uglyXml <- case inputOption of
    (FromFile filePath) -> TIO.readFile filePath
    (FromText xmlText) -> pure xmlText
  let prettyXml = prettyPrintXml prettifyOptions uglyXml
  liftIO $ case outputOption of
    (ToFile filePath) -> TIO.writeFile filePath prettyXml
    _ -> TIO.putStrLn prettyXml

data Options = Options
  { inputOption :: InputOption,
    outputOption :: OutputOption,
    prettifyOptions :: PrettifyOpts
  }

data InputOption = FromFile FilePath | FromText XmlText

data OutputOption = ToFile FilePath | ToConsole

mkOptions :: Parser Options
mkOptions = Options <$> inputOpt <*> outputOpt <*> prettifyOpts
  where
    inputFile :: Parser FilePath
    inputFile =
      strOption
        ( long "file"
            <> short 'f'
            <> metavar "FILE_NAME"
            <> help "XML file to pretty-print"
        )
    inputText :: Parser XmlText
    inputText =
      strOption
        ( long "text"
            <> short 't'
            <> metavar "XML_TEXT"
            <> help "XML text to pretty-print"
        )
    inputOpt = (FromFile <$> inputFile) <|> (FromText <$> inputText)
    outputFile :: Parser FilePath
    outputFile =
      strOption
        ( long "out"
            <> short 'o'
            <> metavar "FILE_NAME"
            <> help "XML file to pretty-print to"
        )
    outputToFile = ToFile <$> outputFile
    outputToConsole :: Parser OutputOption
    outputToConsole =
      flag
        ToConsole
        ToConsole
        ( long "console"
            <> short 'c'
            <> help "Output the pretty-printed XML to the console"
        )
    outputOpt :: Parser OutputOption
    outputOpt = outputToFile <|> outputToConsole
    eol :: Parser EndOfLine
    eol =
      Opt.option
        auto
        ( long "eol"
            <> help "The line-break style: Line Feed (LF), Carriage Return (CR), or both (CRLF)"
            <> showDefault
            <> value LF
            <> metavar "<LF | CR | CRLF>"
        )
    identStyle :: Parser IndentStyle
    identStyle =
      Opt.option
        auto
        ( long "indent-style"
            <> help "The indent style (TAB or SPACE INDENT_SIZE)"
            <> showDefault
            <> value (SPACE 2)
            <> metavar "<TAB | SPACE SIZE>"
        )
    prettifyOpts :: Parser PrettifyOpts
    prettifyOpts = PrettifyOpts <$> identStyle <*> eol

-- Simulates repeated uses for profiling
-- profileSimulate :: IO ()
-- profileSimulate = do
--   uglyXml <- TIO.readFile uglyXmlFile
--   let uglyXmlList = replicate nRuns uglyXml
--   let prettyXmlList = map prettyPrintXmlDefault uglyXmlList
--   pure (deepseq prettyXmlList "Done") >>= putStrLn
--   where
--     uglyXmlFile :: FilePath
--     uglyXmlFile = "data/tests/prettyPrintTest.xml"
--     nRuns = 10000
