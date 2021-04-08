{-# LANGUAGE NoImplicitPrelude #-}

module TestTokenizerUtil where

import RIO
import System.IO
import qualified RIO.Text as T
import qualified Data.Text as T
import qualified Data.List as L
import Prelude (print, read)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex.TDFA
import Data.Array

import System.FilePath
import System.Directory (listDirectory)

-- import Lexer.LexerUtil
-- import Example1.Tokens

data PythonTokenInfo = PythonTokenInfo {
                                       pyTokenType :: !String,
                                       pyTokenString :: !String,
                                       pyTokenStartPos :: !(Int, Int),
                                       pyTokenEndPos:: !(Int, Int)
                                       }

stringToTokenInfo :: String -> PythonTokenInfo
stringToTokenInfo s =
   let token_name = (s =~ "[a-zA-Z]+") :: String
       -- token_string = (s =~ "[\'\"].*[\'\"]") :: String
       -- http://hackage.haskell.org/package/regex-base-0.93.2/docs/Text-Regex-Base-RegexLike.html#t:MatchText
       -- https://stackoverflow.com/questions/5591192/grouping-in-haskell-regular-expressions
       ts_array = getAllTextMatches (s =~ "([\'\"])(.*)([\'\"])") :: Array Int (MatchText String)
       -- Example: " ENCODING   'utf-8'       (0, 0) (0, 0)"
       -- array (0,0) [(0,array (0,3) [(0,("'utf-8'",(12,7))),(1,("'",(12,1))),(2,("utf-8",(13,5))),(3,("'",(18,1)))])]
       token_string = fst $ (ts_array ! 0) ! 2
       offsets = (getAllTextMatches (s =~ "\\([0-9]+, [0-9]+\\)") :: [String])
       offsets' = L.map (\x -> read x :: (Int, Int)) offsets
       start_pos = L.head offsets'
       end_pos = L.head . L.tail $ offsets'
   in
     PythonTokenInfo { pyTokenType=token_name,
                       pyTokenString=token_string,
                       pyTokenStartPos=start_pos,
                       pyTokenEndPos=end_pos
                     }

pythonTokenToTuple = (,,,) <$>
  pyTokenType <*>
  pyTokenString <*>
  pyTokenStartPos <*>
  pyTokenEndPos


testTokenizer lexer tokenInfoTuple file_path = do
  (input_str, expected_tokens) <- parseTestFixture file_path (pythonTokenToTuple . stringToTokenInfo)
  return $ testTokenizer' lexer tokenInfoTuple file_path (T.unpack . T.unlines $ input_str) expected_tokens

testTokenizer' lexer tokenInfoTuple test_name input expected_result = testCase test_name $ do
  let result = lexer (T.unpack . T.strip .  T.strip . T.pack $ input)
  case result of
    Right r -> (L.map tokenInfoTuple $ r) @?= expected_result
    Left e -> assertFailure $ "*****\n" ++ test_name ++ "\n" ++ input ++ (show e) ++ "\n****\n"

parseTestFixture file_path token_constr = do
  contents <- readFileUtf8 file_path
  -- putStrLn $ T.unpack contents
  let _:lines = T.lines contents  -- discard input str
  let input_str = L.init $ L.takeWhile (not . T.isPrefixOf (T.pack "result:")) lines
  let expected_str = L.tail $ L.dropWhile (not . T.isPrefixOf (T.pack "result:")) lines
  let expected_tokens = L.map (token_constr . T.unpack) expected_str
  return (input_str ,expected_tokens)

testTokenizerOnlyTokens lexer tokenInfoToken file_path = do
  (input_str, expected_tokens) <- parseTestFixture file_path stringToTokenInfo
  return $ testTokenizerTokens' lexer tokenInfoToken file_path (T.unpack . T.unlines $ input_str) expected_tokens


testTokenizerTokens' lexer tokenInfoToken test_name input expected_result = testCase test_name $ do
  -- let result = runLexer input
  let result = lexer (T.unpack . T.strip . T.pack $ input)
  case result of
    Right r -> (L.map tokenInfoToken $ r) @?= (L.map (pyTokenType) expected_result)
    Left e -> assertFailure $ "*****\n" ++ test_name ++ "\n" ++ input ++ (show e) ++ "\n****\n"
