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
import Example5.LexerRunner
import Example5.Tokens

main = do
  let base_folder = "./test/test_fixtures/5/"
  allFiles <- listDirectory base_folder

  tests' <-  mapM (testTokenizerOnlyTokens runLexer tokenInfoToken  . (base_folder ++)) allFiles
  tests'' <-  mapM (testTokenizer runLexer tokenInfoTuple . (base_folder ++)) allFiles

  -- defaultMain $ testGroup "tokenizer_tests" $ tests'
  defaultMain $ testGroup "tokenizer_tests_example_5" $ tests' ++ tests''
  --defaultMain tests
