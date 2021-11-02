{-# LANGUAGE LambdaCase #-}

module Expr2.Main where

import Control.Monad (join, liftM2)
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Data.Bifunctor (first, second)

data Tm
  = TmInt Int
  | TmBool Bool
  | TmAdd Tm Tm
  | TmEq Tm Tm
  | TmIfThenElse Tm Tm Tm
  deriving (Show)

i :: Int -> Tm
i = TmInt

b :: Bool -> Tm
b = TmBool

add :: Tm -> Tm -> Tm
add = TmAdd

eq :: Tm -> Tm -> Tm
eq = TmEq

ifte :: Tm -> Tm -> Tm -> Tm
ifte = TmIfThenElse

data Ty
  = TyBool
  | TyInt
  deriving (Show, Eq)

data TyErr
  = TyErrMismatch Ty Ty
  deriving (Show)

type Check = Except TyErr Ty

-- >>> check $ ifte (b True `eq` b True) (i 10 `add` i 20) (i 40)
-- ExceptT (Identity (Right TyInt))

-- >>> check $ ifte (b True `eq` b True) (i 10 `add` i 20) (b False)
-- ExceptT (Identity (Left (TyErrMismatch TyInt TyBool)))
check :: Tm -> Check
check = \case
  TmInt _ -> return TyInt
  TmBool _ -> return TyBool
  TmAdd l r -> check l >>= matches TyInt >> check r >>= matches TyInt
  TmEq l r -> bind2 matches (check l) (check r)
  TmIfThenElse c t e -> check c >>= matches TyBool >> bind2 matches (check t) (check e)
  where
    matches :: Ty -> Ty -> Check
    matches l r
      | l == r = return l
      | otherwise = throwError $ TyErrMismatch l r

data Vl
  = VlBool Bool
  | VlInt Int
  deriving (Show, Eq)

data VlErr
  = VlErrMismatch
  deriving (Show)

type Eval = Except VlErr Vl

-- >>> eval $ ifte (b True `eq` b True) (i 10 `add` i 20) (i 40)
-- ExceptT (Identity (Right (VlInt 30)))

-- >>> eval $ ifte (b True `eq` b True) (i 10 `add` i 20) (b False)
-- ExceptT (Identity (Right (VlInt 30)))
eval :: Tm -> Eval
eval = \case
  TmInt i -> return $ VlInt i
  TmBool b -> return $ VlBool b
  TmAdd l r -> do
    vl <- eval l
    vr <- eval r
    case (vl, vr) of
      (VlInt vl, VlInt vr) -> return . VlInt $ vl + vr
      _ -> throwError VlErrMismatch
  TmEq l r -> do
    vl <- eval l
    vr <- eval r
    return . VlBool $ vl == vr
  TmIfThenElse c t e -> do
    vc <- eval c
    vt <- eval t
    ve <- eval e
    case vc of
      (VlBool True) -> return vt
      (VlBool False) -> return ve
      _ -> throwError VlErrMismatch

-- >>> run $ ifte (b True `eq` b True) (i 10 `add` i 20) (i 40)
run :: Tm -> IO ()
run t = case runExcept $ check t of
  Left e -> putStrLn $ "type-error: " <> show e
  Right _ -> case runExcept $ eval t of
    Left e -> putStrLn $ "eval-error: " <> show e
    Right v -> putStrLn $ "result: " <> show v

bind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = join $ liftM2 f a b

main :: IO ()
main = return ()
