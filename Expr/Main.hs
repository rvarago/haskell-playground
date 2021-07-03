{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Expr.Main where

-- Expr AST.

import Control.Monad
import Control.Monad.Except
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Term language.

data Expr
  = Fl
  | Tr
  | Zero
  | Succ Expr
  | Pred Expr
  | IsZero Expr
  | Let ValName Expr Expr
  | Val ValName
  | If Expr Expr Expr
  deriving (Show, Eq)

type ValName = Text

type ECtx = [(ValName, Expr)]

eval :: Expr -> ECtx -> Maybe Expr
eval e ctx = if isVal (nf e ctx) then return $ nf e ctx else Nothing

nf :: Expr -> ECtx -> Expr
nf e ctx = maybe e (`nf` ctx) (reduce e ctx)

reduce :: Expr -> ECtx -> Maybe Expr
reduce (Succ n) ctx = Succ <$> reduce n ctx
reduce (Pred Zero) _ = return $ Pred Zero
reduce (Pred (Succ n)) _ | isNum n = return n
reduce (Pred n) ctx = Pred <$> reduce n ctx
reduce (IsZero Zero) _ = return Tr
reduce (IsZero (Succ n)) _ | isNum n = return Fl
reduce (IsZero n) ctx = IsZero <$> reduce n ctx
reduce (Let n h b) ctx =
  let ctx' = extendECtx n h ctx
   in reduce b ctx'
reduce (Val n) ctx = lookup n ctx
reduce (If Fl _ e) _ = return e
reduce (If Tr t _) _ = return t
reduce (If c t e) ctx = (\c' -> If c' t e) <$> reduce c ctx
reduce _ _ = Nothing

isVal :: Expr -> Bool
isVal e = isBool e || isNum e

isBool :: Expr -> Bool
isBool = \case
  Fl -> True
  Tr -> True
  _ -> False

isNum :: Expr -> Bool
isNum = \case
  Zero -> True
  Succ n | isNum n -> True
  Pred n | isNum n -> True
  _ -> False

extendECtx :: ValName -> Expr -> ECtx -> ECtx
extendECtx n e ctx = (n, e) : ctx

-- Type language.

data Type
  = TBool
  | TNat
  deriving (Show, Eq)

data TypeError
  = TypeMismatch Type Type
  | TypeValNotFound ValName
  deriving (Show, Eq)

type TCtx = [(ValName, Type)]

typeCheck :: Expr -> TCtx -> Either TypeError Type
typeCheck e ctx = runExcept $ go e ctx
  where
    go :: Expr -> TCtx -> Except TypeError Type
    go Fl _ = return TBool
    go Tr _ = return TBool
    go Zero _ = return TNat
    go (Succ n) ctx =
      go n ctx >>= \case
        TBool -> throwError $ TypeMismatch TNat TBool
        TNat -> return TNat
    go (Pred n) ctx =
      go n ctx >>= \case
        TBool -> throwError $ TypeMismatch TNat TBool
        TNat -> return TNat
    go (IsZero n) ctx =
      go n ctx >>= \case
        TBool -> throwError $ TypeMismatch TNat TBool
        TNat -> return TBool
    go (Let n h b) ctx = go h ctx >>= \th -> go b (extendTCtx n th ctx)
    go (Val n) ctx = maybe (throwError $ TypeValNotFound n) return (lookup n ctx)
    go (If c t e) ctx = do
      tc <- go c ctx
      tt <- go t ctx
      te <- go e ctx
      unless (tc == TBool) $ throwError $ TypeMismatch TBool tc
      unless (tt == te) $ throwError $ TypeMismatch tt te
      return tt

extendTCtx :: ValName -> Type -> TCtx -> TCtx
extendTCtx n e ctx = (n, e) : ctx

-- Frontend.

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#!" "!#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

rword :: Text -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

reserved :: [Text]
reserved = ["#f", "#t", "zero", "pred", "succ", "iszero", "if", "then", "else", "let", "in"]

pProgram :: Parser Expr
pProgram = between sc eof pExpr

pExpr :: Parser Expr
pExpr =
  choice
    [ pVal,
      pBool,
      pNat,
      pIsZero,
      pIf,
      pLet
    ]

pIdent :: Parser Text
pIdent = (lexeme . try) (p >>= check . T.pack)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reserved
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

pVal :: Parser Expr
pVal = Val <$> pIdent

pBool :: Parser Expr
pBool =
  choice
    [ pFalse,
      pTrue
    ]

pFalse :: Parser Expr
pFalse = rword "#f" $> Fl

pTrue :: Parser Expr
pTrue = rword "#t" $> Tr

pNat :: Parser Expr
pNat =
  choice
    [ pZero,
      pPred,
      pSucc
    ]

pZero :: Parser Expr
pZero = rword "zero" $> Zero

pPred :: Parser Expr
pPred = Pred <$> (rword "pred" *> pExpr)

pSucc :: Parser Expr
pSucc = Succ <$> (rword "succ" *> pExpr)

pIsZero :: Parser Expr
pIsZero = IsZero <$> (rword "iszero" *> pExpr)

pIf :: Parser Expr
pIf = do
  rword "if"
  c <- pExpr

  rword "then"
  t <- pExpr

  rword "else"
  If c t <$> pExpr

pLet :: Parser Expr
pLet = do
  rword "let"
  name <- pIdent

  void $ symbol "="
  head <- pExpr

  rword "in"
  Let name head <$> pExpr

-- Checking then running.

run :: Expr -> Either TypeError (Maybe Expr)
run e = typeCheck e tctx $> eval e ectx
  where
    ectx = []
    tctx = []

process :: Expr -> IO ()
process e = case run e of
  Left err -> putStrLn $ "type error> " <> show err
  Right Nothing -> putStrLn $ "eval error> " <> "cannot evaluate term"
  Right (Just e) -> putStrLn $ "ok> " <> show e

-- Examples.

ex1 :: Expr
ex1 = Let "b" (Succ $ Pred $ Succ Zero) (IsZero $ Val "a")

program :: String -> Expr
program t = case parse pProgram "" (T.pack t) of
  Left err -> error $ "parsing: " <> show err
  Right expr -> expr

p1 :: [Char]
p1 = "let x = succ zero in if iszero x then #t else #f"

p2 :: [Char]
p2 = "let x = succ zero in if #t then zero else succ zero"