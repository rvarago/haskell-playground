{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Existential.Main where

-- Existential wrapper with usage via CPS + Rank-N types.

import Data.Kind (Constraint, Type)

-- Existential.

type Some :: (Type -> Constraint) -> Type
data Some c where
  MkSome :: (c a) => a -> Some c

withSome :: forall c r. Some c -> (forall a. c a => a -> r) -> r
withSome (MkSome a) k = k a

-- Usage.

class Interface a where
  act :: a -> String

data InstanceX = InstanceX
  deriving (Show)

instance Interface InstanceX where
  act _ = "this is X"

data InstanceY = InstanceY
  deriving (Show)

instance Interface InstanceY where
  act _ = "this is Y"

data InterfaceSelector
  = SelectX
  | SelectY
  deriving (Show)

mkSomeInstance :: InterfaceSelector -> Some Interface
mkSomeInstance = \case
  SelectX -> MkSome InstanceX
  SelectY -> MkSome InstanceY

program :: InterfaceSelector -> IO ()
program = run . mkSomeInstance
  where
    run :: Some Interface -> IO ()
    run si = withSome si $ putStrLn . act

main :: IO ()
main = program SelectX >> program SelectY