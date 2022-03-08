{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ExprReader.Main where

-- A simple evaluator based on the Reader monad with implicit environment-passing for an even simple language.

import Control.Applicative
import Control.Monad.Reader
import qualified Data.Map as M

-- >>> run
-- Just (VLit 3)
run =
  let expr = cond (var "x") (lit 1 `add` var "y") (bool False)
      env = M.fromAscList [("x", VBool True), ("y", VLit 2)]
   in runReader (eval expr) env

data Expr
  = ELit Int
  | EBool Bool
  | EVar Ident
  | EAdd Expr Expr
  | ECond Expr Expr Expr
  deriving (Show)

lit = ELit

bool = EBool

var = EVar

add = EAdd

cond = ECond

type Ident = String

type Env = M.Map Ident Value

data Value
  = VLit Int
  | VBool Bool
  deriving (Show)

type Eval = Reader Env (Maybe Value)

eval :: Expr -> Eval
eval (ELit i) = wrapLit i
eval (EBool b) = wrapBool b
eval (EVar n) = asks $ M.lookup n
eval (EAdd l r) = join $ liftA2 evalAdd (eval l) (eval r)
eval (ECond c t e) = do
  c <- eval c
  case c of
    Just (VBool True) -> eval t
    Just (VBool False) -> eval e
    _ -> pure Nothing

wrapLit = pure . Just . VLit

wrapBool = pure . Just . VBool

evalAdd (Just (VLit l)) (Just (VLit r)) = wrapLit (l + r)
evalAdd _ _ = pure Nothing
