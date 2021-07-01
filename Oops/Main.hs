{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

module Oops.Main where

-- Faking some non-enterprise-level OoPS with an existential wrapper.

class Speakable a where
  speak :: a -> String

data EPerson where
  EPerson :: Speakable a => a -> EPerson

newtype Adult = Adult
  { adultName :: String
  }
  deriving (Show)

instance Speakable Adult where
  speak (Adult name) = show name <> " says: hi, I'm old"

newtype Child = Child
  { childName :: String
  }
  deriving (Show)

instance Speakable Child where
  speak (Child name) = show name <> " says: hi, I'm young"

speakAll :: [EPerson] -> IO ()
speakAll = mapM_ go
  where
    go (EPerson p) = putStrLn $ speak p

program1 :: IO ()
program1 = speakAll ps
  where
    ps = [EPerson $ Adult "Mr. X", EPerson $ Child "Baby. X"]