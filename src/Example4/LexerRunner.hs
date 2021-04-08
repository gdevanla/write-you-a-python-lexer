{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example4.LexerRunner where

import RIO
import Prelude (print)
import qualified Data.List as L
import qualified Data.Text as T
import Example4.Lexer
import Example4.Tokens

--- Alex related data structures copied from generated Alex file --

runLexer :: String -> Either String [TokenInfo]
runLexer c = runAlex c lexerFold

runLexerOnFile :: FilePath -> IO (Either String [TokenInfo])
runLexerOnFile file_path = do
  contents <- readFileUtf8 file_path
  return $ runAlex (T.unpack  contents) lexerFold

runAndPrintLexer :: String -> IO ()
runAndPrintLexer c = do
  let result = runLexer c
  print result
