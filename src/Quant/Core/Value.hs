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
  Virt :: ValidSelector acs t => QR a t -> Virt a acs t

virtFromR :: ValidSelector (CountTo s) s => QR a s -> Virt a (CountTo s) s
virtFromR = Virt

mkQ :: ValidSelector (CountTo s) s =>
     Basis (NList a s) => [(NList a s, PA)] -> IO (Virt a (CountTo s) s)
mkQ = fmap virtFromR . qrFromList

printQ :: Show a => Virt a acs t -> IO ()
printQ (Virt qr) = do
  printQR qr

selectQ ::
  forall n a acs t. 
    ValidSelector n (Length acs)
     => Virt a acs t -> Virt a (Select n acs) t
selectQ = unsafeCoerce

appV ::
     forall a acs s. Basis (NList a s)
  => Basis a 
  => ValidSelector acs s
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
    ValidSelector '[Eval (s !! n)] t
    => Measureable a (Eval (s !! n)) t
    => Virt a s t -> Key n -> IO (NList a 1)
measureV (Virt qr) Key = observeN qr (SNat @(Eval (s !! n)))