{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Core.Observation
  ( observeV
  , observeRef
  , observeN
  , module Core.QR
  , Measureable
  ) where

import           Core.QR

import           Data.IORef
import           Data.List     (find)
import           System.Random (Random (randomR), getStdRandom)

observeV :: Basis (NList a s) => QV a s -> IO (NList a s)
observeV v = do
  let nv = normalize v
      probs = squareModulus . getProb nv <$> basis
  r <- getStdRandom $ randomR (0.0, 1.0)
  let accumulatedProbs = zip (scanl1 (+) probs) basis
      Just (_, res) = find ((r <) . fst) accumulatedProbs
        -- never yields Nothing due to normalization
  return res

observeRef :: Basis (NList a s) => QR a s -> IO (NList a s)
observeRef (QR ptr) = do
  qVal <- readIORef ptr
  observResult <- observeV qVal
  writeIORef ptr (mkQV [(observResult, 1)])
  return observResult

type Measureable a n s 
  = (Basis (NList a s),
    Basis (NList a (n - 1)), 
    Basis a,
    Basis (NList a (s - n)),
    KnownNat n)

-- indexed at 1
observeN ::
     forall a s n. 
     Measureable a n s
  => ValidSelector '[n] s 
  => QR a s 
  -> SNat n
  -> IO (NList a 1)
observeN (QR ptr) SNat = do
  qVal <- readIORef ptr
  let prob' a =
        sqrt . sum
          $ [ squareModulus
              (getProb qVal $ unsafeCoerce (left <+> (a :> NNil) <+> right))
              :+ 0
            | left <- basis @(NList a (n - 1))
            , right <- basis @(NList a (s - n))
            ]
      auxQval = mkQV [(a :> NNil, prob' a) | a <- basis @a]
  obsRes <- observeV auxQval
  let newVal =
        mkQV
          $ ([ ( unsafeCoerce $ left <+> obsRes <+> right
               , getProb qVal $ unsafeCoerce (left <+> obsRes <+> right))
             | left <- basis @(NList a (n - 1))
             , right <- basis @(NList a (s - n))
             ] :: [(NList a s, PA)])
  writeIORef ptr (normalize newVal)
  return obsRes