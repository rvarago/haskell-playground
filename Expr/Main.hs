{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Expr.Main where

-- Expr AST.

import Data.Functor (($>))
import qualified Data.Text as T
import Expr.Frontend
import Expr.Term
import Expr.Type
import Text.Megaparsec hiding (State)

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

ast1 :: Expr
ast1 = Let "a" (Succ $ Pred $ Succ Zero) (IsZero $ Val "a")

program :: String -> Expr
program t = case parse parser "" (T.pack t) of
  Left err -> error $ "parsing: " <> show err
  Right expr -> expr

source1 :: [Char]
source1 = "let x = succ zero in if iszero x then #t else #f"

source2 :: [Char]
source2 = "let x = succ zero in if #t then zero else succ zero"