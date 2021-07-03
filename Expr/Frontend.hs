{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Expr.Frontend (parser) where

import Control.Monad
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Expr.Term
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

parser :: Parser Expr
parser = between sc eof pExpr

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
