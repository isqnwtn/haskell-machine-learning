{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Net
    ( someFunc
    , Network(..)
    , Weights(..)
    ) where

import Control.Monad.Random
import Data.Singletons
import Prelude.Singletons
import Numeric.LinearAlgebra.Static
import GHC.TypeLits
import GHC.TypeLits.Singletons

data Weights i o = W { wBiases :: !(R o)
                     , wNodes  :: !(L o i)
                     }

data Network :: Nat -> [Nat] -> Nat -> * where
  O     :: !(Weights i o)
        -> Network i '[] o
  (:&~) :: KnownNat h
        => !(Weights i h)
        -> !(Network h hs o)
        -> Network i (h ': hs) o

infixr 5 :&~

randomWeights :: (MonadRandom m, KnownNat i, KnownNat o)
              => m (Weights i o)
randomWeights = do
  s1 :: Int <- getRandom
  s2 :: Int <- getRandom
  let wB = randomVector s1 Uniform * 2 -1
      wN = uniformSample s2 (-1) 1
  return $ W wB wN

randomNet :: forall m i hs o. (MonadRandom m, KnownNat i, SingI hs, KnownNat o)
          => m (Network i hs o)
randomNet = go sing
  where
    go :: forall h hs'. KnownNat h
       => Sing hs'
       -> m (Network h hs' o)
    go = \case
        SNil            ->     O <$> randomWeights
        SNat `SCons` ss -> (:&~) <$> randomWeights <*> go ss

someFunc :: IO ()
someFunc = do
  print "yeet"
