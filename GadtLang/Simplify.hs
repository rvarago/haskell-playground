{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module GadtLang.Simplify (simplify) where

import GadtLang.Lang
  ( Term (TmAdd, TmBool, TmEq, TmIf, TmInt, TmMul),
  )

simplify :: Term a -> Term a
simplify (TmAdd l (TmInt 0)) = simplify l
simplify (TmAdd (TmInt 0) r) = simplify r
simplify (TmMul _ (TmInt 0)) = TmInt 0
simplify (TmMul (TmInt 0) _) = TmInt 0
simplify (TmMul l (TmInt 1)) = simplify l
simplify (TmMul (TmInt 1) r) = simplify r
simplify (TmAdd l r) = simplify $ TmAdd (simplify l) (simplify r)
simplify (TmMul l r) = simplify $ TmMul (simplify l) (simplify r)
simplify (TmEq l r) = simplify $ TmEq (simplify l) (simplify r)
simplify (TmIf (TmBool False) _ e) = simplify e
simplify (TmIf (TmBool True) t _) = simplify t
simplify tm = tm
