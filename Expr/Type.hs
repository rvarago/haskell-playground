{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Expr.Type where

import Control.Monad
import Control.Monad.Except
import Expr.Term (Expr (..), ValName)

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
