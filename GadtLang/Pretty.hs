{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module GadtLang.Pretty (print) where

import GadtLang.Lang
  ( Term (TmAdd, TmBool, TmEq, TmIf, TmInt, TmLet, TmMul, TmVal),
  )
import Text.Printf (printf)
import Prelude hiding (print)

print :: Term a -> String
print (TmBool b) = printf "%s : bool" $ show b
print (TmInt i) = printf "%s : int" $ show i
print (TmVal n) = printf "%s : var" n
print (TmLet n h b) = printf "let %s = %s in %s" n (print h) (print b)
print (TmAdd l r) = printf "(%s + %s)" (print l) (print r)
print (TmMul l r) = printf "(%s * %s)" (print l) (print r)
print (TmEq l r) = printf "(%s == %s)" (print l) (print r)
print (TmIf c t e) = printf "if %s then %s else %s" (print c) (print t) (print e)
