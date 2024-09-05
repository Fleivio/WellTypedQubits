module Core.Value
  ( Virt(..)
  , module Core.Observation
  , mkQ
  , printQ
  , selectQ
  , appV
  , measureV
  ) where

import           Data.IORef
import           Core.Observation
import List.Key

type Virt :: Type -> [Natural] -> Natural -> Type

data Virt a acs t where
  Virt :: ValidDecomposer acs t => QR a t -> Virt a acs t

virtFromR :: ValidDecomposer (CountTo s) s => QR a s -> Virt a (CountTo s) s
virtFromR = Virt

mkQ :: ValidDecomposer (CountTo s) s =>
     Basis (NList a s) => [(NList a s, PA)] -> IO (Virt a (CountTo s) s)
mkQ = fmap virtFromR . qrFromList

printQ :: Show a => Virt a acs t -> IO ()
printQ (Virt qr) = do
  printQR qr

selectQ ::
  forall n a acs t. 
    ValidDecomposer n (Length acs)
     => Virt a acs t -> Virt a (Select n acs) t
selectQ = unsafeCoerce

appV ::
     forall a acs s. Basis (NList a s)
  => Basis a 
  => ValidDecomposer acs s
  => Qop a (Length acs) (Length acs) -> Virt a acs s -> IO ()
appV f' (Virt (QR ptr)) = do
  qv <- readIORef ptr
  let fqv = normalize $ appQop gf qv
  writeIORef ptr fqv
  where
    gf =
      mkQop
        [ ((ua, ub), getOpProb f' (a, b))
        | ua <- basis @(NList a s)
        , ub <- basis @(NList a s)
        , let (a, na) = decompose @acs ua
              (b, nb) = decompose @acs ub
        , na == nb
        ]

measureV ::
    forall a s t n. 
    ValidDecomposer '[s `At` n] t
    => Measureable a (s `At` n) t
    => Virt a s t -> Key n -> IO (NList a 1)
measureV (Virt qr) Key = observeN qr (SNat @(s `At` n))