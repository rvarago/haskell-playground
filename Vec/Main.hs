{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Vec.Main where

-- Length-index vector.
--
-- Overly annotated for clarity.
import GHC.Types (Constraint, Type)
import Prelude hiding (elem, filter, foldr, head, length, map, replicate, tail, take, takeWhile, (++))

-- Nat.

data Nat
  = Zero
  | Succ Nat
  deriving (Show)

type family (n :: Nat) + (m :: Nat) :: Nat where
  'Zero + m = m
  ('Succ n) + m = 'Succ (n + m)

type family Min (n :: Nat) (m :: Nat) :: Nat where
  Min 'Zero _ = 'Zero
  Min ('Succ _) 'Zero = 'Zero
  Min ('Succ n) ('Succ m) = 'Succ (Min n m)

class KnowNothing (n :: Nat)

instance KnowNothing (n :: Nat)

class IsZero (n :: Nat)

instance IsZero 'Zero

class (n :: Nat) >= (m :: Nat)

instance m >= 'Zero

instance (m >= n) => ('Succ m >= 'Succ n)

instance {-# OVERLAPPABLE #-} (m >= n) => ('Succ m >= n)

class IsEven (n :: Nat)

instance IsEven 'Zero

instance (IsEven n) => IsEven ('Succ ('Succ n))

-- Singleton Nat.
data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

toNat :: SNat n -> Nat
toNat SZero = Zero
toNat (SSucc n) = Succ $ toNat n

class ToSNat (n :: Nat) where
  toSNat :: SNat n

instance ToSNat 'Zero where
  toSNat = SZero

instance (ToSNat n) => ToSNat ('Succ n) where
  toSNat = SSucc $ toSNat @n

deriving stock instance Show (SNat n)

-- Vec.

data Vec (n :: Nat) (a :: Type) where
  VNil :: Vec 'Zero a
  (:>) :: a -> Vec n a -> Vec ('Succ n) a

infixr 5 :>

deriving stock instance Show a => Show (Vec n a)

data SomeVec (c :: Nat -> Constraint) (a :: Type) :: Type where
  MkSomeVec :: c n => Vec n a -> SomeVec c a

deriving stock instance Show a => Show (SomeVec c a)

empty :: SomeVec IsZero a
empty = MkSomeVec VNil

head :: Vec ('Succ n) a -> a
head (x :> _) = x

tail :: Vec ('Succ n) a -> Vec n a
tail (_ :> xs) = xs

foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldr _ x0 VNil = x0
foldr f x0 (x :> xs) = f x (foldr f x0 xs)

map :: (a -> b) -> Vec n a -> Vec n b
map _ VNil = VNil
map f (x :> xs) = f x :> map f xs

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
VNil ++ ys = ys
(x :> xs) ++ ys = x :> (xs ++ ys)

length :: Vec n a -> SNat n
length VNil = SZero
length (_ :> xs) = SSucc $ length xs

take :: SNat n -> Vec m a -> Vec (Min n m) a
take SZero _ = VNil
take (SSucc _) VNil = VNil
take (SSucc n) (x :> xs) = x :> take n xs

elem :: Eq a => a -> Vec n a -> Bool
elem e = foldr step False
  where
    step x found = found || x == e

replicate :: SNat n -> a -> Vec n a
replicate SZero _ = VNil
replicate (SSucc n) x = x :> replicate n x

filterMap :: (a -> Maybe b) -> Vec n a -> SomeVec ((>=) n) b
filterMap _ VNil = MkSomeVec VNil
filterMap p (x :> xs)
  | Just x <- p x = case filterMap p xs of MkSomeVec xs -> MkSomeVec $ x :> xs
  | otherwise = case filterMap p xs of MkSomeVec xs -> MkSomeVec xs

filter :: (a -> Bool) -> Vec n a -> SomeVec ((>=) n) a
filter p = filterMap $ toMaybe p

toMaybe :: (a -> Bool) -> (a -> Maybe a)
toMaybe p x
  | p x = Just x
  | otherwise = Nothing

fromList :: [a] -> SomeVec KnowNothing a
fromList [] = MkSomeVec VNil
fromList (x : xs) = case fromList xs of MkSomeVec xs -> MkSomeVec $ x :> xs

toList :: Vec n a -> [a]
toList = foldr (:) []

-- Examples.

withEvenLength :: (IsEven n, Show a) => Vec n a -> IO ()
withEvenLength xs = putStrLn $ "Called with even length vector: " <> show xs

mapInts :: Vec n Integer -> Vec n String
mapInts = map (show . (+ 1))

natsIso :: Nat
natsIso = toNat (toSNat @'Zero)

mkVecLen2 :: [a] -> Maybe (Vec ('Succ ('Succ 'Zero)) a)
mkVecLen2 (a : b : _) = Just $ a :> b :> VNil
mkVecLen2 _ = Nothing