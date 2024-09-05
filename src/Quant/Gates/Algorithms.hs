module Gates.Algorithms(adder, deutsch, teleport) where

import Gates.QAct
import Core.Value
import List.Key

adder :: Basis (NList Bit t)
  => ValidDecomposer '[n1, n2, carryIn, carryOut] t
  => ValidDecomposer '[n1, n2, carryOut] t
  => ValidDecomposer '[n2, carryIn] t
  => ValidDecomposer '[n1, n2] t
  => ValidDecomposer '[n2, carryIn, carryOut] t
  => QAct '[n1, n2, carryIn, carryOut] t
adder = do
  app @'[1,2,4] toffoli
  app @'[1,2] cnot
  app @'[2,3,4] toffoli
  app @'[2,3] cnot
  app @'[1,2] cnot

deutsch :: Basis (NList Bit t)
  => ValidDecomposer '[n1, n2] t
  => ValidDecomposer '[n1] t
  => ValidDecomposer '[n2] t
  => Measureable Bit n1 t
  => QAct '[n1, n2] t -> QAct '[n1, n2] t
deutsch uf = do
  app @'[1] h
  app @'[2] h
  app @'[1,2] uf
  app @'[1] h
  val <- measure @1
  case val of
    O -> liftIO $ print "f is constant"
    I -> liftIO $ print "f is balanced"

teleport :: Basis (NList Bit t)
  => ValidDecomposer '[n1, n2, n3] t
  => ValidDecomposer '[n1, n2] t
  => ValidDecomposer '[n2, n3] t
  => ValidDecomposer '[n1, n3] t
  => ValidDecomposer '[n1] t
  => ValidDecomposer '[n2] t
  => Measureable Bit n1 t
  => Measureable Bit n2 t
  => QAct '[n1, n2, n3] t
teleport = do
  app @'[2,3] entangle
  app @'[1,2] cnot
  app @'[1] h
  measure @1
  measure @2
  app @'[2,3] cnot
  app @'[1,3] cz
