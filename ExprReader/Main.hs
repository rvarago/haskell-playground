{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ExprReader.Main where

-- A simple evaluator based on the Reader monad with implicit environment-passing for an even simple language.

import Control.Applicative
import Control.Monad.Reader
import qualified Data.Map as M

-- >>> run
-- Just (VBool False)
run =
  let expr = letin "y" (lit 1 `add` lit 2) (cond (var "x") (lit 100 `add` var "y") (bool False))
      env = M.fromAscList [("x", VBool True)]
   in eval env expr

data Expr
  = ELit Int
  | EBool Bool
  | EVar Ident
  | ELetIn Ident Expr Expr
  | EAdd Expr Expr
  | ECond Expr Expr Expr
  deriving (Show)

lit = ELit

bool = EBool

var = EVar

add = EAdd

letin = ELetIn

cond = ECond

type Ident = String

type Env = M.Map Ident Value

data Value
  = VLit Int
  | VBool Bool
  deriving (Show)

type Eval = Reader Env (Maybe Value)

eval env = flip runReader env . eval'

eval' :: Expr -> Eval
eval' (ELit i) = wrapLit i
eval' (EBool b) = wrapBool b
eval' (EVar n) = asks $ M.lookup n
eval' (ELetIn n h b) =
  eval' h >>= \case
    Just h -> local (M.insert n h) $ eval' b
    Nothing -> evalFail
eval' (EAdd l r) = join $ liftA2 evalAdd (eval' l) (eval' r)
eval' (ECond c t e) =
  eval' c >>= \case
    Just (VBool True) -> eval' t
    Just (VBool False) -> eval' e
    _ -> evalFail

wrapLit = pure . Just . VLit

wrapBool = pure . Just . VBool

evalAdd (Just (VLit l)) (Just (VLit r)) = wrapLit (l + r)
evalAdd _ _ = evalFail

evalFail = pure Nothing
