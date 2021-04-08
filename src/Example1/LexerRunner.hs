{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example1.LexerRunner where

import RIO
import Prelude (print)
import qualified Data.List as L
import qualified Data.Text as T
import Example1.Lexer
import Example1.Tokens

--- Alex related data structures copied from generated Alex file --

runLexer :: String -> Either String [TokenInfo]
runLexer code = Right $ lexer code

runLexerOnFile :: FilePath -> IO (Either String [TokenInfo])
runLexerOnFile file_path = do
  contents <- readFileUtf8 file_path
  return $ runLexer (T.unpack  contents)

runAndPrintLexer :: String -> IO ()
runAndPrintLexer c = do
  let result = runLexer c
  print result
