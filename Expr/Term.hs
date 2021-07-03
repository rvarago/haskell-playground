{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Expr.Term (Expr (..), ECtx, eval, ValName) where

import Data.Text (Text)

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
