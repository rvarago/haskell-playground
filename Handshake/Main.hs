{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Handshake.Main where

-- Handshake type-state.
--
-- Overly annotated for clarity.

import GHC.Types (Type)

-- OCaml style, off we go :).
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

data State
  = Closed
  | Negotiating
  | Authenticating
  | Connected
  deriving (Show)

data SState (s :: State) :: Type where
  SClosed :: SState 'Closed
  SNegotiating :: SState 'Negotiating
  SAuthenticating :: SState 'Authenticating
  SConnected :: SState 'Connected

deriving stock instance Show (SState s)

class SStateI (s :: State) where
  sstate :: SState s

instance SStateI 'Closed where
  sstate = SClosed

instance SStateI 'Negotiating where
  sstate = SNegotiating

instance SStateI 'Authenticating where
  sstate = SAuthenticating

instance SStateI 'Connected where
  sstate = SConnected

type Address = String

data Handshake (s :: State) :: Type where
  HOpen :: Address -> SState 'Closed -> Handshake 'Negotiating
  HAuthenticate :: Address -> SState 'Negotiating -> Handshake 'Authenticating
  HAccept :: Address -> SState 'Authenticating -> Handshake 'Connected
  HClose :: Handshake 'Closed

deriving stock instance Show (Handshake s)

open :: Address -> Handshake 'Negotiating
open addr = HOpen addr SClosed

auth :: Handshake 'Negotiating -> Handshake 'Authenticating
auth (HOpen addr _) = HAuthenticate addr SNegotiating

accept :: Handshake 'Authenticating -> Handshake 'Connected
accept (HAuthenticate addr _) = HAccept addr SAuthenticating

close :: Closeable s => Handshake s -> Handshake 'Closed
close _ = HClose

connect :: forall s. (SStateI s, IsOpen s ~ 'True) => (Handshake s -> Handshake 'Connected)
connect = case sstate @s of
  SNegotiating -> connect . auth
  SAuthenticating -> accept
  SConnected -> id

class (IsOpen s ~ 'True) => Closeable (s :: State)

instance IsOpen s ~ 'True => Closeable s

type family IsOpen (s :: State) :: Bool where
  IsOpen 'Closed = 'False
  IsOpen _ = 'True

data SomeHandshake where
  MkSomeHandshake :: Handshake s -> SomeHandshake

deriving stock instance Show SomeHandshake

fromState :: Address -> SState s -> SomeHandshake
fromState _ SClosed = MkSomeHandshake HClose
fromState addr SNegotiating = MkSomeHandshake $ HOpen addr SClosed
fromState addr SAuthenticating = MkSomeHandshake $ HAuthenticate addr SNegotiating
fromState addr SConnected = MkSomeHandshake $ HAccept addr SAuthenticating

program1 :: Handshake 'Connected
program1 = "localhost:8080" |> open |> connect

program2 :: SomeHandshake
program2 = MkSomeHandshake (HAccept "localhost:8080" SAuthenticating)
