{-# LANGUAGE LambdaCase #-}

module Unsat.Main where

-- A simple, possibly buggy, solver for boolean-valued expressions.

import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S

data Term
  = TFalse
  | TTrue
  | TVar Identifier
  | TNot Term
  | TAnd Term Term
  | TOr Term Term
  deriving (Show)

type Identifier = String

type Vars = S.Set Identifier

-- >>> variables $ TNot (TVar "x") `TAnd` TVar "x" `TOr` TVar "y" `TAnd` TVar "z"
-- fromList ["x","y","z"]
variables :: Term -> Vars
variables = go S.empty
  where
    go ctx = \case
      TFalse -> ctx
      TTrue -> ctx
      TVar i -> S.insert i ctx
      TNot t -> go ctx t
      TAnd l r -> go ctx l <> go ctx r
      TOr l r -> go ctx l <> go ctx r

type Valuation = M.Map Identifier Bool

-- >>> valuations $ S.fromAscList ["x", "y", "z"]
-- [fromList [("x",False),("y",False),("z",False)],fromList [("x",False),("y",False),("z",True)],fromList [("x",False),("y",True),("z",False)],fromList [("x",False),("y",True),("z",True)],fromList [("x",True),("y",False),("z",False)],fromList [("x",True),("y",False),("z",True)],fromList [("x",True),("y",True),("z",False)],fromList [("x",True),("y",True),("z",True)]]
valuations :: Vars -> [Valuation]
valuations = map M.fromAscList . mapM values . S.toAscList
  where
    values i = [(i, False), (i, True)]

-- >>> eval (M.fromAscList [("x", False)]) $ TNot (TVar "x")
-- True
--
-- >>> eval (M.fromAscList [("x", True)]) $ TNot (TVar "x")
-- False
eval :: Valuation -> Term -> Bool
eval vals = \case
  TFalse -> False
  TTrue -> True
  TVar i -> fromMaybe (error $ "var not found: " <> i) (M.lookup i vals)
  TNot t -> evalUni not t
  TAnd l r -> evalBin (&&) l r
  TOr l r -> evalBin (||) l r
  where
    evalUni op t = op $ eval vals t
    evalBin op l r = eval vals l `op` eval vals r

-- >>> solve $ TVar "x" `TAnd` TVar "x"
-- [(False,fromList [("x",False)]),(True,fromList [("x",True)])]
--
-- >>> solve $ TNot (TVar "x")
-- [(True,fromList [("x",False)]),(False,fromList [("x",True)])]
--
-- >>> solve $ (TNot (TVar "x") `TAnd` (TVar "x") `TOr` TVar "y") `TAnd` TVar "z"
-- [(False,fromList [("x",False),("y",False),("z",False)]),(False,fromList [("x",False),("y",False),("z",True)]),(False,fromList [("x",False),("y",True),("z",False)]),(True,fromList [("x",False),("y",True),("z",True)]),(False,fromList [("x",True),("y",False),("z",False)]),(False,fromList [("x",True),("y",False),("z",True)]),(False,fromList [("x",True),("y",True),("z",False)]),(True,fromList [("x",True),("y",True),("z",True)])]
solve :: Term -> [(Bool, Valuation)]
solve t = map step $ valuations (variables t)
  where
    step val = (eval val t, val)

-- >>> models $ TVar "x" `TAnd` TVar "y"
-- [fromList [("x",True),("y",True)]]
--
-- >>> models $ TNot (TVar "x")
-- [fromList [("x",False)]]
--
-- >>> models $ (TNot (TVar "x") `TAnd` (TVar "x") `TOr` TVar "y") `TAnd` TVar "z"
-- [fromList [("x",False),("y",True),("z",True)],fromList [("x",True),("y",True),("z",True)]]
models :: Term -> [Valuation]
models = mapMaybe satisfied . solve
  where
    satisfied (sat, val)
      | sat = return val
      | otherwise = Nothing
