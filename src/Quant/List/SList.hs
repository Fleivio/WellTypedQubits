module List.SList
  ( SList(..), SNat(..), sListToList,  sListCountTo, Length, CountTo, Select, Eval, type (!!)
  ) where

import           Data.Kind
import           Data.Type.Equality
import Data.Proxy
import           Fcf                hiding (type (+), Length, type (-),
                                     type (<=))
import           Fcf.Data.List hiding (Length, Elem)
import           GHC.TypeLits
import Unsafe.Coerce

type SList :: [Natural] -> Type
data SList acs where
  SNil  :: SList '[]
  (:>>) :: SNat a -> SList acs -> SList (a ': acs)
infixr 5 :>>

instance Show (SList acs) where
  show sl = "@" <> show (sListToList sl)

(<++>) :: SList as -> SList bs -> SList (Eval (as ++ bs))
SNil <++> bs = bs
(a :>> as) <++> bs = a :>> (as <++> bs)

sListToList :: SList acs -> [Int]
sListToList SNil         = []
sListToList ((SNat :: SNat a) :>> as) = fromIntegral (natVal (SNat @a)) : sListToList as

sListCountTo :: SNat n -> SList (CountTo n)
sListCountTo (SNat :: SNat n) = go (Proxy @n)
  where
    go :: Proxy n -> SList (CountTo n)
    go p = case natVal p of
      0 -> unsafeCoerce SNil
      n -> unsafeCoerce $ case someNatVal (n-1) of
            Just (SomeNat (Proxy :: Proxy m)) -> unsafeCoerce $ (sListCountTo $ (SNat @m)) <++> (SNat @n :>> SNil)



data ECountTo :: Natural -> Exp [Natural]
type instance Eval (ECountTo n) = CountToImpl n

type CountTo (n :: Natural) = Eval (ECountTo n)

type family CountToImpl (n :: Natural) :: [Natural]
 where
  CountToImpl 0 = '[]
  CountToImpl n = Eval (CountToImpl (n - 1) ++ '[ n])

type family Length (as :: [a]) :: Natural
 where
  Length '[]      = 0
  Length (a : as) = 1 + Length as

data ESelect :: [s] -> [Natural] -> Exp [s]
type instance Eval (ESelect '[] ns) = '[]
type instance Eval (ESelect (x ': xs) ns) = Eval (ns !! x) ': Eval (ESelect xs ns)

type Select acs ns = Eval (ESelect acs ns)

data (!!) :: [s] -> Natural -> Exp s
type instance Eval ('[] !! n) = Stuck
type instance Eval ((x ': xs) !! n)
  = If (n == 1) x (Eval (xs !! (n - 1)))


-- data Maximum :: [Natural] -> Exp Natural

-- type instance Eval (Maximum xs) = MaximumImpl xs
-- type family MaximumImpl (a :: [Natural]) :: Natural
--  where
--   MaximumImpl '[]       = TypeError (Text "Unable to Eval Maximum of a empty list")
--   MaximumImpl (x : '[]) = x
--   MaximumImpl (x : xs)  = If (x <=? MaximumImpl xs) (MaximumImpl xs) x

-- data Elem :: Natural -> [Natural] -> Exp Bool
-- type instance Eval (Elem a as) = ElemImpl a as
-- type family ElemImpl (a :: Natural) (as :: [Natural]) :: Bool
--  where
--   ElemImpl a '[]      = 'False
--   ElemImpl a (a ': as) = 'True
--   ElemImpl a (b ': as) = ElemImpl a as

-- data HasRepetition :: [Natural] -> Exp Bool
-- type instance Eval (HasRepetition '[]) = 'False
-- type instance Eval (HasRepetition (x ': xs)) = If (Eval (Elem x xs)) 'True (Eval (HasRepetition xs))

-- data HasZero :: [Natural] -> Exp Bool
-- type instance Eval (HasZero '[]) = 'False
-- type instance Eval (HasZero (x ': xs)) = If (x == 0) 'True (Eval (HasZero xs))



-- data PowerSet :: [s] -> Exp [[s]]
-- type instance Eval (PowerSet '[]) = '[ '[]]
-- type instance Eval (PowerSet (x ': xs)) 
--   = Eval ( (++) (Eval (PowerSet xs)) =<< (Map (Cons x) =<< PowerSet xs))




-- data DropAt :: Natural -> [s] -> Exp [s]
-- type instance Eval (DropAt n xs) = Eval (Eval (Take n xs) ++ Eval (Drop (n + 1) xs))

-- ----------------------------------------------------------------

-- type BoundCheck (n :: Natural) (xs :: [Natural]) 
--   = If (Eval (Maximum xs) <=? n) (() :: Constraint) 
--     (TypeError (
--         Text "Index out of bounds on Qubit selection" 
--         :$$: 
--         Text "You got " :<>: ShowType n :<>: Text " qubits" :$$: Text "But tried to select qubits " :<>: ShowType xs
--         ))

-- type NoCloningCheck (xs :: [Natural]) 
--   = If (Eval (HasRepetition xs)) 
--     (TypeError (
--         Text "No Cloning Theorem Violation" 
--         :$$: 
--         Text "You tried to select qubits with repetition " :<>: ShowType xs
--         ))
--     (() :: Constraint) 

-- type NoZeroCheck (xs :: [Natural]) 
--   = If (Eval (HasZero xs)) 
--     (TypeError (
--         Text "Zero qubit selection is not allowed" 
--         :$$:
--         Text "The qubit selection list starts from 1"
--         ))
--     (() :: Constraint)

-- data EValidSelector :: Natural -> [Natural] -> Exp Constraint
-- type instance Eval (EValidSelector size acs)
--   = Eval (Constraints [BoundCheck size acs,
--                        NoCloningCheck acs,
--                        NoZeroCheck acs,
--                        ToListOfInts acs])

-- type ValidSelector acs size = Eval (EValidSelector size acs)

