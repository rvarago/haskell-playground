module GadtLang.Main where

import GadtLang.Context
import GadtLang.Eval
import GadtLang.Lang
import GadtLang.Pretty
import GadtLang.Simplify
import Prelude hiding (print)

e1 = TmAdd (TmInt 10) (TmInt 0)

e2 = TmAdd (TmInt 10) (TmInt 0)

e3 = TmLet "x" (TmInt 20) (TmAdd (TmInt 10) (TmVal "x"))

e4 = TmIf (TmVal "x" `TmEq` TmVal "y") (TmInt 10) (TmInt 20)