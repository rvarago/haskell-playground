{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module GadtLang.Context (Ctx, Handler, empty, lookup, insert, fromList) where

import Control.Monad.Reader (ReaderT)
import qualified Data.Map as M
import Prelude hiding (lookup)

type Ctx k v = M.Map k v

type Handler ctx v = ReaderT ctx Maybe v

empty :: Ctx k v
empty = M.empty

lookup :: Ord k => k -> Ctx k v -> Maybe v
lookup = M.lookup

insert :: Ord k => k -> v -> Ctx k v -> Ctx k v
insert = M.insert

fromList :: Ord k => [(k, v)] -> Ctx k v
fromList = M.fromList