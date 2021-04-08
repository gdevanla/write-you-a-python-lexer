{
{-# LANGUAGE OverloadedStrings#-}

module Example2.Lexer where

import Example2.LexerUtil
import Example2.Tokens
import qualified Data.Text as T
import qualified Data.List as L

}

-- %wrapper "basic"

$digit = 0-9   -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$white_no_nl = [\ \t]

@identifier = $alpha [$alpha $digit \_]*

-- aligns with break up defined in
-- https://docs.python.org/3/reference/lexical_analysis.html#integer-literals
$nonzerodigit = [1..9]
$bindigit     =  [01]
$octdigit     = [0..7]
@hexdigit     =  $digit | [a-f] | [A-F]
@decinteger = (0(_?0)* | [1-9](_?[0-9])*)
@hexinteger   = ([0][xX](_?[0-9a-fA-F])+)
@bininteger = (0[bB](_?[01])+)
@octinteger = (0[oO](_?[0-7])+)
@intnumber    =  @decinteger | @bininteger | @octinteger | @hexinteger

@digitpart     =  $digit([_]|$digit)*
@fraction      =  [\.] @digitpart
@pointfloat    =  (@digitpart)* @fraction | @digitpart[\.]

@exponent      =  [eE] ([\+\-]?) @digitpart
@exponentfloat =  (@digitpart | @pointfloat)* @exponent

@floatnumber   =  @pointfloat | @exponentfloat

@imagnumber    =  (@floatnumber | @digitpart) [jJ]
@number = @imagnumber | @floatnumber | @intnumber
---End number related rules

-- start string related rules
-- adopted from https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals
$lf = \n  -- line feed
$cr = \r  -- carriage return
@eol_pattern = $lf | $cr $lf | $cr $lf

@stringprefix    =  r | u | R | U | f | F
                     | fr | Fr | fR | FR | rf | rF | Rf | RF
-- @single = ([^'\\]*((\\.|\\\n)[^'\\]*)*')
$short_str_char = [^ \n \r ' \" \\]
$shortstringchar_nosinglequote = [^ ' \\ \n]
$shortstringchar_nodoublequote = [^ \" \\ \n]
@stringescapeseq =  [\\](\\|'|\"|@eol_pattern|$short_str_char)
@shortstringitem_single =  $shortstringchar_nosinglequote | @stringescapeseq
@shortstringitem_double =  $shortstringchar_nodoublequote | @stringescapeseq
@shortstring     =  ' @shortstringitem_single* ' | \" @shortstringitem_double* \"

$longstringchar  = [. \n] # [\\]
@longstringitem  =  $longstringchar | @stringescapeseq
@longstring      =  (\'\'\' @longstringitem* \'\'\') | (\"\"\" @longstringitem* \"\"\")

@stringliteral   =  (@stringprefix)* (@shortstring | @longstring)


-- the tokenizer currently except non-ascii and emite Synxtax error later
@bytesescapeseq =  [\\][\0-\127]
@shortbyteschar_single =  [\0-\127] # [\\ \n \']
@shortbyteschar_double =  [\0-\127] # [\\ \n \"]
@shortbytesitem_single =  @shortbyteschar_single | @bytesescapeseq
@shortbytesitem_double =  @shortbyteschar_double | @bytesescapeseq
@shortbytes     =  ' @shortbytesitem_single* ' | \" @shortbytesitem_double* \"

@longbyteschar =  [\0-\127] # [\\]
@longbytesitem  =  @longbyteschar | @bytesescapeseq
@longbytes      =  \'\'\' @longbytesitem* \'\'\' | \"\"\" @longbytesitem* \"\"\"


@bytesprefix    =  b | B | br | Br | bR | BR | rb | rB | Rb | RB
@bytesliteral   =  @bytesprefix (@shortbytes | @longbytes)

--- end of string and byte literal macros


tokens :-
       @number {action Number}
       @identifier {action Name}
       @stringliteral | @bytesliteral {action String}
       $white+ ;     -- this ignores new lines as well

{

-- adapted from https://www.haskell.org/alex/doc/html/wrappers.html
-- alexScanTokens :: String -> [TokenInfo]
lexer str = go ('\n',[],str)
  where go inp@(_,_bs,str) =
          case alexScan inp 0 of
            AlexEOF -> []
            AlexError _ -> error $ "Lexical error" ++ show inp
            AlexSkip  inp' len     -> go inp'
            AlexToken inp' len act -> act inp len : go inp'

}
