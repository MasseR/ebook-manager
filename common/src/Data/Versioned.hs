{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
module Data.Versioned where

import GHC.TypeLits
import ClassyPrelude
import Data.Generics.Product

newtype Versioned (v :: Nat) a = Versioned a deriving (Show)

instance Functor (Versioned v) where
  fmap f (Versioned a) = Versioned (f a)

instance Applicative (Versioned v) where
  pure = Versioned
  (Versioned f) <*> (Versioned a) = Versioned (f a)

class Migrate a b | b -> a where
  migrate :: a -> b
  default migrate :: (Subtype b a) => a -> b
  migrate = upcast
