{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
module Net
    ( someFunc
    ) where

import Data.Massiv.Array
import GHC.TypeLits

data Vect a
data Matr a
type R = Double

newtype Dimen (n :: Nat) t = Dimen t

newtype Arr n = A (Dimen n (Vect R))
newtype Mat m n = M (Dimen m (Dimen n (Matr R)))


data Weights i o = W { wBiases :: !(Arr o)
                     , wNodes  :: !(Mat o i)
                     }

data Network :: Nat -> [Nat] -> Nat -> * where
  O     :: !(Weights i o)
        -> Network i '[] o
  (:&~) :: KnownNat h
        => !(Weights i h)
        -> !(Network h hs o)
        -> Network i (h ': hs) o

infixr 5 :&~

someFunc :: IO ()
someFunc = do
  val <- evaluateM vec 4
  print val
  where
    vec = makeVectorR D Seq 10 id
