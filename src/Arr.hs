-- |
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module Arr where

import Data.Massiv.Array as A
import Numeric.LinearAlgebra.Static
import GHC.TypeLits


data Vect a
data Matr a
type X = Double

newtype Dimen (n :: Nat) t = Dimen t

newtype Arr n = A (Dimen n (Vect X))
newtype Mat m n = M (Dimen m (Dimen n (Matr X)))
