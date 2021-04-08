{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example4.LexerRunner where

import RIO
import Text.Printf
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
runAndPrintLexer c = runAndPrettyPrintLexer c


runAndPrettyPrintLexer :: String -> IO ()
runAndPrettyPrintLexer c = do
  let result = runLexer c
  case result of
    Right toks -> printf "%s" $ L.unlines $ L.map formatTokenInfo toks
    Left e -> print e

formatTokenInfo :: TokenInfo -> String
formatTokenInfo tokInfo = let
  name = printf "%-10s" (show . token_type $ tokInfo) :: String
  string = printf "%-10s" (show . token_string $ tokInfo) :: String
  loc = printf "%d,%d-%d,%d:" (fst . start_pos $ tokInfo) (snd . start_pos $ tokInfo) (fst . end_pos $ tokInfo) (snd . end_pos $ tokInfo) :: String
  in
    -- printf "%s%s%s%s" name string start_pos end_pos
    printf "%-20s %s %s" loc name string
