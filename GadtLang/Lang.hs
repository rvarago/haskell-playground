{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module GadtLang.Lang (Name, Term (..), Type (..)) where

data Term (a :: Type) where
  TmBool :: Bool -> Term 'TyBool
  TmInt :: Int -> Term 'TyInt
  TmLet :: Name -> Term a -> Term b -> Term b
  TmVal :: Name -> Term a
  TmAdd :: Term 'TyInt -> Term 'TyInt -> Term 'TyInt
  TmMul :: Term 'TyInt -> Term 'TyInt -> Term 'TyInt
  TmEq :: Term a -> Term a -> Term 'TyBool
  TmIf :: Term 'TyBool -> Term a -> Term a -> Term a

deriving stock instance Show (Term a)

type Name = String

data Type
  = TyBool
  | TyInt
  deriving (Show, Eq)
