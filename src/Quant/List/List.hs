module List.List
  ( NList(..), (<+>)
  , module Data.Kind
  , module GHC.TypeLits
  , module Unsafe.Coerce
  , module Data.Type.Equality
  , decompose
  , module List.SList
  ) where

import List.NList

import           Data.Kind
import           Data.Proxy
import           Data.Type.Equality
import           Fcf                hiding (Length, type (+), type (-),
                                     type (<=))
import           GHC.TypeLits
import           Unsafe.Coerce
import          List.SList



decompose' :: [Int] -> [a] -> ([a], [a])
decompose' slist nlist = (selectionList, restList)
  where
    selectionList = flip (!!) . pred <$> slist <*> pure nlist
    restList      = map snd $ filter ((`notElem` slist) . fst) $ [1 ..] `zip` nlist

decompose ::
     forall acs n a. ValidSelector acs n
  => NList a n
  -> (NList a (Length acs), NList a (n - Length acs))
decompose nlist = (unsafeCoerce selectionList, unsafeCoerce restList)
  where
    term_level_sList          = toListOfInts @acs
    term_level_nList          = toList nlist
    (selectionList, restList) = decompose' term_level_sList term_level_nList



