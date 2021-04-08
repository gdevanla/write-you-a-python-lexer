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

import TestTokenizerUtil
import Example1.LexerRunner
import Example1.Tokens

main = do
  let base_folder = "./test/test_fixtures/1/"
  allFiles <- listDirectory base_folder
  tests1 <-  mapM (testTokenizer runLexer tokenInfoTuple . (base_folder ++)) allFiles
  tests11 <-  mapM (testTokenizerOnlyTokens runLexer tokenInfoToken  . (base_folder ++)) allFiles

  let all_tests = tests1 ++ tests11

  -- defaultMain $ testGroup "tokenizer_tests" $ tests'
  defaultMain $ testGroup "tokenizer_tests_example_1" all_tests
  --defaultMain tests
