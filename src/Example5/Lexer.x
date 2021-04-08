{
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE RecordWildCards#-}

module Example5.Lexer where

import Example5.LexerUtil
import Example5.Tokens
import qualified Data.Text as T
import qualified Data.List as L

}

-- %wrapper "basic"

$digit = 0-9   -- digits
$alpha = [a-zA-Z_]  -- alphabetic characters
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

$not_single_quote = [. \n] # '
$not_double_quote = [. \n] # \"
@one_single_quote = ' $not_single_quote
@two_single_quotes = '' $not_single_quote
@one_double_quote = \" $not_double_quote
@two_double_quotes = \"\" $not_double_quote

$longstringchar  = [. \n] # [' \"]
@longstringitem_single  =  $longstringchar | @stringescapeseq | @one_single_quote | @two_single_quotes | \"
@longstringitem_double  =  $longstringchar | @stringescapeseq | @one_double_quote | @two_double_quotes | \'
@longstring      =  (''' @longstringitem_single* ''') | (\"\"\" @longstringitem_double* \"\"\")

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
$newline = [\r \n]                                                                                                                                                                                                 $not_newline = ~$newline
@commentline = (($white_no_nl)*  \# ($not_newline)*)
@empty_line = (($white_no_nl)*\n)


tokens :-
       @number {action Number}
       @identifier {action Name}
       @stringliteral | @bytesliteral {action String}

       \n$white* {startWhite}
       $white+ ;   -- ignote this since we only care up significant white spaces (leading white spaces)

       $white+ ;     -- this ignores new lines as well

      @empty_line {action Nl}                                                                                                                                                                                            @commentline {commentAction}

      "("   { openParen Lpar }
      ")"   { closeParen Rpar }
      "["   { openParen Lsqb }
      "]"   { closeParen Rsqb }
      "{"   { openParen Lbrace }
      "}"   { closeParen Rbrace }
      "..." { action Ellipsis}
      "->"  { action RArrow }
      "."   { action Dot }
      "~"   { action Tilde }
      "+"   { action Plus }
      "-"   { action Minus }
      "**"  { action DoubleStar }
      "*"   { action Star }
      "/"   { action Slash }
      "//"  { action DoubleSlash }
      "%"   { action Percent }
      "<<"  { action LeftShift }
      ">>"  { action RightShift }
      "<"   { action Less }
      "<="  { action LessEqual }
      ">"   { action Greater }
      ">="  { action GreaterEqual }
      "=="  { action EqEqual }
      "!="  { action NotEqual }
      "^"   { action Circumflex }
      "|"   { action VBar }
      "&"   { action Amper }
      ":"   { action Colon }
      "="   { action Equal }
      ":="  { action ColonEqual }
      "+="  { action PlusEqual }
      "-="  { action MinEqual }
      "*="  { action StarEqual }
      "/="  { action SlashEqual }
      "%="  { action PercentEqual }
      "**=" { action DoubleStarEqual }
      "&="  { action AmperEqual }
      "|="  { action VBarEqual }
      "^="  { action CircumflexEqual }
      "<<=" { action LeftShiftEqual }
      ">>=" { action RightShiftEqual }
      "//=" { action DoubleSlashEqual }
      "@="  { action AtEqual }
      ","   { action Comma }
      "@"   { action At }
      \;    { action Semi }

{

/* This version left here as a reference from Example3 */
/* adapted from https://www.haskell.org/alex/doc/html/wrappers.html */
/* alexScanTokens :: String -> [TokenInfo] */
/* lexer str = go (alexStartPos, '\n',[],str) */
/*   where go inp@(posn, _,_bs,str) = */
/*           case alexScan inp 0 of */
/*             AlexEOF -> [] */
/*             AlexError _ -> error $ "Lexical error" ++ show inp ++ "at location=" ++ (show posn) */
/*             AlexSkip  inp' len     -> go inp' */
/*             AlexToken inp' len act -> act inp len : go inp' */

/* lexer :: Alex TokenInfo  -- This is our state monad */
/* lexer = do */
/*       inp <- alexGetInput */
/*       case alexScan inp 0 of */
/*         AlexEOF -> alexEOF */
/*         AlexError ((AlexPosn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column) */
/*         AlexSkip  inp' _len -> do */
/*           alexSetInput inp' */
/*           lexer -- skip and loop around */
/*         AlexToken inp' len action -> do */
/*           alexSetInput inp' */
/*           action inp len */

lexer :: Alex TokenInfo
lexer = do
  userState <- alexGetUserState
  case userStatePendingTokens userState of
    t:ts -> do
        alexSetUserState $ userState {userStatePendingTokens=ts}
        return t
    [] -> do
      inp <- alexGetInput
      sc <- alexGetStartCode
      case alexScan inp sc of
        AlexEOF -> alexEOF
        AlexError ((AlexPosn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
        AlexSkip  inp__' _len -> do
          alexSetInput inp__'
          lexer
        AlexToken inp' len action -> do
          alexSetInput inp'
          action inp len

/* version before supporting newlines */
/* -- adopted from language-python */
/* lexerFold :: Alex [TokenInfo] */
/* lexerFold = loop [] */
/*   where */
/*     loop toks = do */
/*       token_info@TokenInfo {..} <- lexer */
/*       case token_type of */
/*         EOF -> return $ L.reverse toks */
/*         _ -> loop (token_info : toks) */

-- adopted from language-python
lexerFold :: Alex [TokenInfo]
lexerFold = loop []
  where
    loop toks = do
      token_info@TokenInfo {..} <- lexer
      userState <- alexGetUserState
      case token_type of
        EOF -> do
          let dedents = remainingDedents userState start_pos
          -- we don't generate the ENDMARKER token to keep tests simple. This will be needed for mathing the Python tokenizer
          return $ L.reverse toks ++ dedents --  ++ [endMarkerTokenInfo start_pos]
        _ -> loop (token_info : toks)



-- from generated Alex file
runAlex :: String -> Alex a -> Either String a
runAlex inp (Alex f) =
  case f
    ( AlexState
        { alex_pos = alexStartPos,
          alex_inp = inp,
          alex_chr = '\n',
          alex_bytes = [],
          alex_ust = alexInitUserState,
          alex_scd = 0
        }
    ) of
    Left msg -> Left msg
    Right (_, a) -> Right a

}
