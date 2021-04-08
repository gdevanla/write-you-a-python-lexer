{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE MultiWayIf#-}

module Example3.LexerUtil where

import RIO
import qualified Data.Text as T
import Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.List as L

import Example3.Tokens

-- first step is to decide what our AlexInput type is going to be.
-- Going with general practices(wrapper examples), we will have the following

type Byte = Word8

-- adapted from https://www.haskell.org/alex/doc/html/wrappers.html
data AlexPosn = AlexPosn !Int  -- absolute character offset
                       !Int  -- line number
                       !Int  -- column number
                       deriving (Show, Eq)
type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- rest of the bytes for the current char
                  String)       -- current input string


alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (pn, c,b:bs,s) = Just (b,(pn, c,bs,s))
alexGetByte (_, _,[],[]) = Nothing
alexGetByte (p,_,[],c:s)  = let
  p' = alexMove p c
  in
  case encode [c] of
    b : bs -> p' `seq` Just (b, (p', c, bs, s))
    [] -> error $ "Not byte returned for " ++ show c

alexStartPos = AlexPosn 0 1 1

-- adapter from code generated from Alex (remove change for \t, since Python counts this as offset=1)
alexMove :: AlexPosn -> Char -> AlexPosn
-- alexMove (AlexPn a l c) '\t' = AlexPosn (a+1)  l     ((c+8-1) `div` 8*8+1)  -- tabsize=8
alexMove (AlexPosn a l _) '\n' = AlexPosn (a+1) (l+1)   1
alexMove (AlexPosn a l c) _    = AlexPosn (a+1)  l     (c+1)

-- We dont' use it in our examples
-- alexInputPrevChar :: AlexInput -> Char


type AlexAction = Token -> AlexInput -> Int -> TokenInfo

action :: AlexAction
action tok inp inp_len = let
  (AlexPosn _ line col, c, _, s) = inp
  in
    TokenInfo
    {
      token_type = tok,
      token_string=T.pack (take inp_len s),
      start_pos=(line, col),
      end_pos=(line, col+inp_len-1)
    }
