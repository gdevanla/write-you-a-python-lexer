{
{-# LANGUAGE OverloadedStrings#-}

module Example1.Lexer where

import Example1.LexerUtil
import Example1.Tokens
import qualified Data.Text as T
import qualified Data.List as L

}

-- %wrapper "basic"

$digit = 0-9   -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$white_no_nl = [\ \t]

@identifier = $alpha [$alpha $digit \_]*

@decnumber = (0(_?0)* | [1-9](_?[0-9])*)

@number = @decnumber

tokens :-
       @number {action Number}
       @identifier {action Name}
       $white+ ;     -- this ignores new lines as well

{

-- adapted from https://www.haskell.org/alex/doc/html/wrappers.html
-- alexScanTokens :: String -> [TokenInfo]
lexer str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
            AlexEOF -> []
            AlexError _ -> error "lexical error"
            AlexSkip  inp' len     -> go inp'
            AlexToken inp' len act -> act inp len : go inp'

}
