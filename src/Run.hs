{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf#-}
module Run (run) where

import Import
import RIO
import Data.Text.IO
import qualified Data.Text as T
import Options.Applicative.Simple

import Example1.LexerRunner as Ex1
import Example2.LexerRunner as Ex2
import Example3.LexerRunner as Ex3
import Example4.LexerRunner as Ex4
import Example5.LexerRunner as Ex5

-- | Command line arguments
data AppArgs = AppArgs
  { appExampleName :: !String
  , appFileName :: !(Maybe T.Text)
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


run :: RIO App ()
run = do
 options <- liftIO $ execParser opts

 case (appFileName options) of
   Just f -> do
     let lexer = getFileLexer (appExampleName options)
     contents <- liftIO $ readFileUtf8 . appExampleName $ options
     liftIO $ lexer . T.unpack $ contents
     -- logInfo $ display $ T.pack . show $ result
   Nothing -> return () -- logInfo $ T.pack "nothing"
     -- let lexer = getLexer (appExampleName options)
     -- input <- liftIO $ getContents
     -- let result = lexer (T.unpack input)
     -- logInfo $ display $ T.pack . show $ result
   where
     opts = info (argParser <**> helper)
           ( fullDesc
             <> progDesc "Lexer for a Python program"
             <> header "" )
     -- getLexer "Example1" = Ex1.runLexer
     -- getLexer "Example2" = Ex2.runLexer
     -- getLexer "Example3" = Ex3.runLexer
     -- getLexer "Example4" = Ex4.runLexer
     -- getLexer "Example5" = Ex5.runLexer

     -- getFileLexer "Example1" = Ex1.runAndPrintLexer
     -- getFileLexer "Example2" = Ex2.runAndPrintLexer
     -- getFileLexer "Example3" = Ex3.runAndPrintLexer
     -- getFileLexer "Example4" = Ex4.runAndPrintLexer
     getFileLexer :: String -> (String -> IO ())
     getFileLexer "Example5" = Ex5.runAndPrintLexer
