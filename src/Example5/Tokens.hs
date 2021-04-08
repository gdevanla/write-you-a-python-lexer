{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE MultiWayIf#-}

module Example5.Tokens where

import RIO
import qualified Data.Text as T
import Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.List as L

data TokenInfo = TokenInfo {
    token_type:: Token
  , token_string:: T.Text
  , start_pos:: (Int, Int)
  , end_pos:: (Int, Int)
  }
  deriving (Show, Eq)

--- Used for testing and
tokenInfoTuple :: TokenInfo -> (String, String, (Int, Int), (Int, Int))
tokenInfoTuple tok = (,,,) <$>
  T.unpack . T.toUpper . T.pack . show . token_type <*>
  T.unpack . token_string' <*>
  start_pos <*>
  end_pos $ tok
  where
    token_string' tok = let
      token = token_string tok
      token' = T.replace (T.pack "\\") (T.pack "\\\\") token
      token'' = T.replace (T.pack "\n") (T.pack "\\n") token'
      in
        token''

-- Used for testing
tokenInfoToken :: TokenInfo -> String
tokenInfoToken =  T.unpack . T.toUpper . T.pack . show . token_type

data Token =
      Endmarker
    | Name
    | Number
    | String
    | Newline
    | Indent
    | Dedent
    | Lpar
    | Rpar
    | Lsqb
    | Rsqb
    | Colon
    | Comma
    | Semi
    | Plus
    | Minus
    | Star
    | Slash
    | VBar
    | Amper
    | Less
    | Greater
    | Equal
    | Dot
    | Percent
    | Lbrace
    | Rbrace
    | EqEqual
    | NotEqual
    | LessEqual
    | GreaterEqual
    | Tilde
    | Circumflex
    | LeftShift
    | RightShift
    | DoubleStar
    | PlusEqual
    | MinEqual
    | StarEqual
    | SlashEqual
    | PercentEqual
    | AmperEqual
    | VBarEqual
    | CircumflexEqual
    | LeftShiftEqual
    | RightShiftEqual
    | DoubleStarEqual
    | DoubleSlash
    | DoubleSlashEqual
    | At
    | AtEqual
    | RArrow
    | Ellipsis
    | ColonEqual
    | Op
    | Await
    | Async
    | Type_Ignore
    | Type_Comment
    | Errortoken -- These Aren'T Used By The C Tokenizer But Are Needed For Tokenize.Py
    | Comment
    | Encoding
    | N_Tokens
    | Nt_Offset   -- Special Definitions For Cooperation With Parser
    | Nl
    | EOF
    deriving (Show, Eq)
