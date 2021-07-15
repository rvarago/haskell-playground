{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module GadtLang.Eval (runEval, runEvalWithCtx, Value (..)) where

import Control.Monad.Reader
  ( MonadReader (local),
    MonadTrans (lift),
    ReaderT (ReaderT, runReaderT),
  )
import qualified GadtLang.Context as C
import GadtLang.Lang (Name, Term (..), Type (TyInt))

data Value
  = VBool Bool
  | VInt Int
  deriving (Show)

type Ctx = C.Ctx Name Value

type Eval = C.Handler Ctx Value

runEval :: Term a -> Maybe Value
runEval = runEvalWithCtx C.empty

runEvalWithCtx :: Ctx -> Term a -> Maybe Value
runEvalWithCtx ctx tm = runReaderT (eval tm) ctx

eval :: Term a -> Eval
eval (TmBool b) = return $ VBool b
eval (TmInt i) = return $ VInt i
eval (TmVal n) = ReaderT $ C.lookup n
eval (TmLet n h b) = do
  vh <- eval h
  local (C.insert n vh) (eval b)
eval (TmAdd l r) = evalBiIntOp (+) l r
eval (TmMul l r) = evalBiIntOp (*) l r
eval (TmEq l r) = evalEqOp l r
eval (TmIf c t e) = do
  vc <- eval c
  case vc of
    VBool True -> eval t
    VBool False -> eval e
    _ -> lift Nothing

evalBiIntOp :: (Int -> Int -> Int) -> Term 'TyInt -> Term 'TyInt -> Eval
evalBiIntOp f l r = do
  vl <- eval l
  vr <- eval r
  lift $ case (vl, vr) of
    (VInt vl', VInt vr') -> Just $ VInt $ f vl' vr'
    _ -> Nothing

evalEqOp :: Term a -> Term a -> Eval
evalEqOp l r = do
  vl <- eval l
  vr <- eval r
  case (vl, vr) of
    (VBool l, VBool r) -> lift . Just . VBool $ l == r
    (VInt l, VInt r) -> lift . Just . VBool $ l == r
    _ -> lift Nothing
