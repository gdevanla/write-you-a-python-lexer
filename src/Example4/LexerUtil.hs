{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE MultiWayIf#-}

module Example4.LexerUtil where

import RIO
import qualified Data.Text as T
import Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.List as L

import Example4.Tokens

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


--- The following definitions support the state machinery we need to for handling context-sensitive features ---
data AlexUserState = AlexUserState {
       userStateStartCode :: !Int,
       userStateIndentStack :: [Int],
       userStatePendingTokens :: [TokenInfo],
       userStateParenStack:: [Token],
       userStatePrevComment :: Bool,
       userStatePrevToken :: Token
     }
     deriving (Show)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {
                  userStateStartCode=0,
                  userStateIndentStack=[1],
                  userStatePendingTokens=[],
                  userStateParenStack=[],
                  userStatePrevComment=False,  -- TODO: This is redundant we could just use userStatePrevToken
                  userStatePrevToken=Newline
                  }

-- The Alex state monad
data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],   -- rest of the bytes for the current char
        alex_scd :: !Int,       -- the current startcode
        alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
    }

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return = pure

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
        Right (s, (pos,c,bs,inp__))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp)
 = Alex $ \s -> Right (s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp}, ())


alexEOF :: Alex TokenInfo
alexEOF = do
   (AlexPosn _ line col,c, rest,s) <- alexGetInput
   let start_pos = (line, col)
   let end_pos = (line, col)
   return $ TokenInfo {
               token_type=EOF,
               token_string=T.pack s,
               start_pos=start_pos,
               end_pos=end_pos
               }

--- End of helper function for AlexState


-- Version from Example4 left here for reference
-- type AlexAction = Token -> AlexInput -> Int -> TokenInfo
-- action :: AlexAction
-- action tok inp inp_len = let
--   (AlexPosn _ line col, c, _, s) = inp
--   in
--     TokenInfo
--     {
--       token_type = tok,
--       token_string=T.pack (take inp_len s),
--       start_pos=(line, col),
--       end_pos=(line, col+inp_len-1)
--     }


constructToken :: Token -> (AlexPosn, b1, c1, String) -> Int -> (AlexPosn, b2, c2, d) -> TokenInfo
constructToken tok inp inp_len n_inp = let
    (AlexPosn _ line col, _, _,s) = inp
    (AlexPosn _ nline ncol, _c, _rest, _s) = n_inp -- new input in state

    start_pos = (line, col - 1)
    tok_str = T.pack $ take inp_len s
    end_pos = (nline, ncol -1)
    in
      TokenInfo {
              token_type=tok,
              token_string=tok_str,
              start_pos=start_pos,
              end_pos=end_pos
              }

type AlexAction result = AlexInput -> Int -> Alex result

action :: Token -> AlexAction TokenInfo
action tok inp inp_len = do
       -- this has new updated input
       n_inp@(AlexPosn _ nline ncol,c, rest,s) <- alexGetInput
       return $ constructToken tok inp inp_len n_inp
