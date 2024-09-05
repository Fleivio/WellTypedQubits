module Gates.QAct(QAct, h, cnot, liftIO, entangle, x, y, z, p, t, s, cz, toffoli, fredkin, swap, sample, measure, app, runQ) where

import Control.Monad.Reader
import Gates.Gates
import Core.Value
import List.Key

type QAct' :: [Natural] -> Natural -> Type -> Type
type QAct' acs t a = ReaderT (Virt Bit acs t) IO a

type QAct :: [Natural] -> Natural -> Type
type QAct acs t = QAct' acs t ()

runQ :: ValidDecomposer (CountTo t) t
  => Basis (NList Bit t)
  => QAct (CountTo t) t -> Virt Bit (CountTo t) t  -> IO ()
runQ = runReaderT 

actQop ::
     forall acs s. Basis (NList Bit s)
  => ValidDecomposer acs s
  => Qop Bit (Length acs) (Length acs) -> QAct acs s
actQop f' = do
  vv <- ask
  lift $ appV f' vv

app :: forall newacs acs t. 
  ValidDecomposer newacs (Length acs)
  => QAct (Select newacs acs) t 
  -> QAct acs t
app act = do
  qv <- ask
  let adapterQv = selectQ @newacs qv
  lift $ runReaderT act
                    adapterQv

--------------------------------

-- type Unroll :: [Natural] -> Type -> Constraint
-- class Unroll ns t where
--   type Params ns t :: Type

--   unroll :: t -> Params ns t

-- instance Unroll '[] t where
--   type Params '[] t = t

--   unroll :: t -> t
--   unroll = id

-- instance (Unroll ns t, KnownNat n) => Unroll (n ': ns) t where
--   type Params (n ': ns) t = Key n -> Params ns t

--   unroll :: t -> Key n -> Params ns t
--   unroll t Key = unroll @ns t


-- app' :: forall newacs acs t. 
--   Unroll newacs (QAct acs t)
--   => ValidDecomposer newacs (Length acs)
--   => QAct (Select newacs acs) t 
--   -> Params newacs (QAct acs t)
-- app' act = unroll @newacs @(QAct acs t) $ do
--   qv <- ask
--   let adapterQv = selectQ @newacs qv
--   lift $ runReaderT act
--                     adapterQv

-- entangle2 :: forall n1 n2 s.
--   Basis (NList Bit s) 
--   => ValidDecomposer '[n1, n2] s
--   => ValidDecomposer (Select '[1,2] '[n1, n2]) s
--   => ValidDecomposer '[n1] s
--   => QAct '[n1, n2] s
-- entangle2 = do
--   app' cnot #1 #2

--------------------------------

x :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => QAct '[n] s
x = actQop @'[n] _x

y :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => QAct '[n] s
y = actQop @'[n] _y

z :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => QAct '[n] s
z = actQop @'[n] _z

p :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => Double -> QAct '[n] s
p l = actQop @'[n] (_p l)

t :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => QAct '[n] s
t = actQop @'[n] _t

s :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => QAct '[n] s
s = actQop @'[n] _s

h :: forall n s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n] s 
  => QAct '[n] s
h = actQop @'[n] _h

cnot :: forall n1 n2 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2] s 
  => QAct '[n1, n2] s
cnot = actQop @'[n1, n2] _cnot

cz :: forall n1 n2 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2] s 
  => QAct '[n1, n2] s
cz = actQop @'[n1, n2] _cz

entangle :: forall n1 n2 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2] s
  => ValidDecomposer '[n1] s -- fix that
  => QAct '[n1, n2] s
entangle = do
  app @'[1] h
  app @'[1,2] cnot

fredkin :: forall n1 n2 n3 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2, n3] s 
  => QAct '[n1, n2, n3] s
fredkin = actQop @'[n1, n2, n3] _fredkin

toffoli :: forall n1 n2 n3 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2, n3] s 
  => QAct '[n1, n2, n3] s
toffoli = actQop @'[n1, n2, n3] _toffoli

swap :: forall n1 n2 s.
  Basis (NList Bit s) 
  => ValidDecomposer '[n1, n2] s 
  => QAct '[n1, n2] s
swap = actQop @'[n1, n2] _swap

sample :: QAct acs t
sample = do
  qr <- ask
  liftIO $ printQ qr

measure :: forall n acs s.
  Measureable Bit (At acs n) s
  => ValidDecomposer '[acs `At` n] s
  => QAct' acs s Bit
measure = do 
  qv <- ask
  liftIO $ do 
    (k:>NNil) <- measureV qv (Key @n)
    return k