{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE MultiWayIf#-}

module Example5.LexerUtil where

import RIO
import qualified Data.Text as T
import Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.List as L

import Example5.Tokens

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

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
        Right (s, (pos,c,bs,inp__))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp)
 = Alex $ \s -> Right (s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp}, ())


--- Setter/Getters for AlexUserState -----


alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexSetPrevComment :: Bool  -> Alex ()
alexSetPrevComment v = do
  userState <- alexGetUserState
  alexSetUserState $ userState {userStatePrevComment=v}

alexSetPrevToken :: Token -> Alex ()
alexSetPrevToken tok = do
  userState <- alexGetUserState
  alexSetUserState $ userState {userStatePrevToken = tok}

alexSetParenStack :: [Token] -> Alex ()
alexSetParenStack ps = do
  userState <- alexGetUserState
  alexSetUserState $ userState {userStateParenStack = ps}

alexSetPendingTokens :: [TokenInfo] -> Alex ()
alexSetPendingTokens toks = do
  userState <- alexGetUserState
  alexSetUserState $ userState {userStatePendingTokens = toks}

alexSetIndentStack :: [Int] -> Alex ()
alexSetIndentStack is = do
  userState <- alexGetUserState
  alexSetUserState $ userState {userStateIndentStack = is}

alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())

---- End of Setter/Getters for AlexUserState

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
-- earlier version in the example, then we add special handling for Indent, NewLine
-- constructToken :: Token -> (AlexPosn, b1, c1, String) -> Int -> (AlexPosn, b2, c2, d) -> TokenInfo
-- constructToken tok inp inp_len n_inp = let
--     (AlexPosn _ line col, _, _,s) = inp
--     (AlexPosn _ nline ncol, _c, _rest, _s) = n_inp -- new input in state

--     start_pos = (line, col - 1)
--     tok_str = T.pack $ take inp_len s
--     end_pos = (nline, ncol -1)
--     in
--       TokenInfo {
--               token_type=tok,
--               token_string=tok_str,
--               start_pos=start_pos,
--               end_pos=end_pos
--               }


constructToken :: Token -> (AlexPosn, b1, c1, String) -> Int -> (AlexPosn, b2, c2, d) -> TokenInfo
constructToken tok inp inp_len n_inp = let
    (AlexPosn _ line col, _, _,s) = inp
    (AlexPosn _ nline ncol, _c, _rest, _s) = n_inp -- new input in state

    start_pos = if tok == Indent then (nline, 0) else (line, col -1) -- to align with how Python tokenizer sets up offsets
    tok_str = if | (tok == Indent) -> if T.isSuffixOf "\t" (T.pack . take inp_len $ s) then T.tail . T.pack . take inp_len $ s else T.replicate (ncol - 1) " "
                 | (tok == Newline || tok == Nl) -> T.pack "\n"
                 | otherwise -> T.pack (take inp_len s)

    end_pos = case tok of
                Newline -> (line, col)
                Nl -> (line, col)
                Indent -> (nline, T.length tok_str)
                _ -> (nline, ncol - 1)
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
       let token_info = constructToken tok inp inp_len n_inp
       userState <- alexGetUserState
       alexSetUserState $ userState {userStatePrevToken=token_type token_info}
       return token_info


newLineAction :: AlexAction Token
newLineAction inp inp_len = do
  is_prev_comment <- userStatePrevComment <$> alexGetUserState
  if is_prev_comment then (do
   alexSetPrevComment False
   return Nl) else return Newline

-- | Constructs NL tokens for empty lines and returns last pos after construction
constructNLTokens :: Int -> Int -> [TokenInfo]
constructNLTokens start_line count = let
   derived_lines = [start_line .. (start_line + count -1)]
   construct_token line_no = TokenInfo
       { token_type=Nl,
         token_string="\n",
         start_pos=(line_no, 0),
         end_pos=(line_no, 1)
       }
   in
     L.map construct_token derived_lines

startWhite:: AlexAction TokenInfo
startWhite inp inp_len = do
     is <- userStateIndentStack <$> alexGetUserState
     let cur = case is of
           c:_ -> c
           _ -> error "Indentation stack is not set, alteast one element should be present. Empty list found"

     parenDepth <- length . userStateParenStack <$> alexGetUserState
     let (AlexPosn _ line _, _, _,s) = inp
     -- new input
     n_inp@(AlexPosn _ nline ncol, _, _, ns) <- alexGetInput

     newline_tok <- newLineAction inp inp_len
     userState <- alexGetUserState

     if | (parenDepth > 0) ->  action Newline inp inp_len
        | T.isPrefixOf "#" (T.pack ns) -> do  -- this means we are on a comment only line
            alexSetPrevToken newline_tok
            return (constructToken newline_tok inp inp_len n_inp)    -- no INDENTS on comment only new line
        | otherwise -> do
          -- at this point we have an indentation, but this
          -- indentation could starting on different line based on
          -- preceding empty lines. For all precedin empty lines we
          -- will insert an NL token
          let parts = L.map T.length . T.splitOn "\n" . T.pack . take inp_len $ s
          let pos = L.last parts + 1
          let nl_tokens = constructNLTokens
                          -- the first new line will be returned at
                          -- end of this function
                          (line + 1)
                          -- ["\n", ....., ""] therefore adjust for
                          -- the 2 items (first \n and last
                          -- non-newline)
                          (L.length parts - 2)
          when (pos > cur) $
                 alexSetUserState $ userState {
            userStateIndentStack = pos:is,
            -- takes care of adding the preceding new lines as well
            userStatePendingTokens=nl_tokens ++ [constructToken Indent inp inp_len n_inp]}
          when (pos < cur) $ do
                 let (pre, post) = span (> pos) is
                 let top = case post of
                       t:_ -> t
                       [] -> error $ unwords ["Invalid indent with cur= ", show cur]
                 if pos == top then
                    alexSetUserState $ userState {
                          userStateIndentStack = post,
                          userStatePendingTokens=nl_tokens ++ map (const (dedentTokenInfo (line, pos-1))) pre}
                 else
                   error $ "Invalid indents : " ++ "pos = " ++ show pos ++ " top= " ++ show top ++ "userState = " ++ show userState ++ "pre = " ++ show pre ++ "post = " ++ show post
          when (pos == cur) $
                  alexSetUserState $ userState {
                  userStatePendingTokens=nl_tokens}

          -- set prev token
          alexSetPrevToken newline_tok
          return (constructToken newline_tok inp inp_len n_inp)


-- Para handling - adopted from language-python library
openParen:: Token -> AlexAction TokenInfo
openParen token inp inp_len = do
           userState <- alexGetUserState
           alexSetUserState $ userState {
                            userStateParenStack=token:userStateParenStack userState}
           action token inp inp_len


closeParen:: Token -> AlexAction TokenInfo
closeParen token inp inp_len = do
       userState <- alexGetUserState
       let topParent = userStateParenStack userState
       case topParent of
            t:ts -> case matchParen t token of
            -- TODO: Add line number info
                 False -> error $ "Parens don't match " ++ show t ++ "and" ++ show token
                 True -> do
                       -- pop the stack
                       alexSetUserState $ userState {
                                  userStateParenStack=ts}
                       action token inp inp_len
            -- TODO: Add line number info
            [] -> error $ "No paren's to pop for " ++ show token


matchParen :: Token -> Token -> Bool
matchParen t1 t2 = case (t1, t2) of
           (Lpar, Rpar) -> True
           (Lsqb, Rsqb) -> True
           (Lbrace, Rbrace) -> True
           (_, _) -> False


commentAction :: AlexAction TokenInfo
commentAction inp inp_len = do
  prevToken <-  userStatePrevToken <$> alexGetUserState
  let prevCommentFlag = prevToken == Nl || prevToken == Newline
  alexSetPrevComment prevCommentFlag

  -- this has new updated input
  let (AlexPosn _ line col,c, rest,s) = inp

  let token_string = T.stripStart . T.pack $ take inp_len s
  let new_len = T.length token_string

  let new_pos = col + inp_len - new_len - 1
  let start_pos = (line, new_pos)
  let end_pos = (line, new_pos + new_len)

  return $ TokenInfo {
    token_type=Comment,
    token_string=token_string,
    start_pos=start_pos,
    end_pos=end_pos
    }

endMarkerTokenInfo :: (Int, b) -> TokenInfo
endMarkerTokenInfo start_pos = TokenInfo
  {
  token_type=Endmarker,
  token_string="",
  start_pos=(fst start_pos + 1, 0),
  end_pos=(fst start_pos + 1, 0)
  }


dedentTokenInfo :: (Int, Int) -> TokenInfo
dedentTokenInfo start_pos = TokenInfo
  {
  token_type=Dedent,
  token_string="",
  start_pos=(fst start_pos + 1, snd start_pos),
  end_pos=(fst start_pos + 1, snd start_pos)
}


remainingDedents :: AlexUserState -> (Int, Int) -> [TokenInfo]
remainingDedents state start_pos = let
  indents = userStateIndentStack state
  in
  map (const $ dedentTokenInfo (fst start_pos, 0)) (L.tail indents)
