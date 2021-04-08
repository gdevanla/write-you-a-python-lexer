{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE MultiWayIf#-}

module Example2.LexerUtil where

import RIO
import qualified Data.Text as T
import Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.List as L

import Example2.Tokens

-- first step is to decide what our AlexInput type is going to be.
-- Going with general practices(wrapper examples), we will have the following

type Byte = Word8
type AlexInput = (Char, [Byte], String)

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,b:bs,s) = Just (b,(c,bs,s))
alexGetByte (_,[],[]) = Nothing
alexGetByte (_,[],c:s)  =  case encode [c] of
    b : bs -> Just (b, (c, bs, s))
    [] -> error $ "Not byte returned for " ++ show c


-- We dont' use it in our examples
-- alexInputPrevChar :: AlexInput -> Char


type AlexAction = Token -> AlexInput -> Int -> TokenInfo

action :: AlexAction
action tok inp inp_len = let
  (c, _, s) = inp
  in
    TokenInfo
    {
      token_type = tok,
      token_string=T.pack (take inp_len s),
      start_pos=(0, 0),
      end_pos=(0, 0)
    }
