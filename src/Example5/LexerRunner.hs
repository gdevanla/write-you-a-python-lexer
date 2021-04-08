{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example5.LexerRunner where

import Prelude (print)
import RIO
import qualified Data.List as L
import qualified Data.Text as T
import Example5.Lexer
import Example5.Tokens

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
