{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Data.Text as T

import Prelude (getContents)
import Example1.LexerRunner as Ex1
import Example2.LexerRunner as Ex2
import Example3.LexerRunner as Ex3
import Example4.LexerRunner as Ex4
import Example5.LexerRunner as Ex5


-- | Command line arguments
data AppArgs = AppArgs
  { appExampleName :: !String
  , appFileName :: !(Maybe String)
  } deriving (Show)

argParser = AppArgs
  <$> strOption ( long "example_name"
                  <> metavar "Example Name"
                  <> help "Specify one of Example1, Example2 etc"
                )
  <*> optional (strOption ( long "file_name"
                  <> metavar "FILE_NAME"
                  <> help "Path of file name to run the Lexer on"
                ))

main :: IO ()
main = do
  options <- execParser opts

  case (appFileName options) of
    Just f -> do
      let lexer = getFileLexer (appExampleName options)
      contents <- readFileUtf8 f
      liftIO $ lexer . T.unpack $ contents
     -- logInfo $ display $ T.pack . show $ result
    Nothing -> do
      contents <- getContents
      let lexer = getFileLexer (appExampleName options)
      liftIO $ lexer  contents
     -- let result = lexer (T.unpack input)
     -- logInfo $ display $ T.pack . show $ result
    where
      opts = info (argParser <**> helper)
           ( fullDesc
             <> progDesc "Lexer for a Python program"
             <> header "" )
      getFileLexer :: String -> (String -> IO ())
      getFileLexer "Example1" = Ex1.runAndPrintLexer
      getFileLexer "Example2" = Ex2.runAndPrintLexer
      getFileLexer "Example3" = Ex3.runAndPrintLexer
      getFileLexer "Example4" = Ex4.runAndPrintLexer
      getFileLexer "Example5" = Ex5.runAndPrintLexer
