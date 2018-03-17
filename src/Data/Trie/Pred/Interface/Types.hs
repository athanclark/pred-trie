{-# LANGUAGE
    GADTs
  , TypeOperators
  , TypeFamilies
  , KindSignatures
  , DataKinds
  , RankNTypes
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  , ConstraintKinds
  , OverloadedStrings
  , OverloadedLists
  #-}


{- |
Module      : Data.Trie.Pred.Interface.Types
Copyright   : (c) 2015 Athan Clark

License     : BSD-style
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC
-}

module Data.Trie.Pred.Interface.Types
  ( -- * Heterogenous Construction
    Singleton (..)
  , Extend (..)
  , Extrude (..)
  , ExtrudeSoundly
  , CatMaybes
  , -- * Path Construction
    only
  , pred
  , (./)
  , nil
  , -- * Path Types
    PathChunk
  , PathChunks
  ) where


import Prelude hiding (pred)
import           Data.Trie.Pred.Base (RootedPredTrie (..), PredTrie (..), emptyPT)
import           Data.Trie.Pred.Base.Step (Pred (..), PredStep (..))
import qualified Data.Trie.HashMap as HT
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.Function.Poly (ArityTypeListIso)
import Data.Typeable (Typeable)

import Data.String (IsString (..))


-- * Classes

-- | Convenience type-level function for removing 'Nothing's from a type list.
type family CatMaybes (xs :: [Maybe *]) :: [*] where
  CatMaybes '[]               = '[]
  CatMaybes ('Nothing  ': xs) =      CatMaybes xs
  CatMaybes (('Just x) ': xs) = x ': CatMaybes xs

-- | Creates a string of nodes - a trie with a width of 1.
class Singleton chunks a trie | chunks a -> trie where
  singleton :: chunks -> a -> trie

-- Basis
instance Singleton (PathChunks k '[]) a (RootedPredTrie k a) where
  singleton Nil r = RootedPredTrie (Just r) emptyPT

-- Successor
instance ( Singleton (PathChunks k xs) new trie0
         , Extend (PathChunk k x) trie0 trie1
         ) => Singleton (PathChunks k (x ': xs)) new trie1 where
  singleton (Cons u us) r = extend u $! singleton us r


-- | Turn a list of tries (@Rooted@) into a node with those children
class Extend eitherUrlChunk child result | eitherUrlChunk child -> result where
  extend :: eitherUrlChunk -> child -> result

-- | Literal case
instance ( Eq k
         , Hashable k
         ) => Extend (PathChunk k 'Nothing) (RootedPredTrie k a) (RootedPredTrie k a) where
  extend (Lit t) (RootedPredTrie mx xs) = RootedPredTrie Nothing $
    PredTrie (HT.HashMapStep $! HM.singleton t (HT.HashMapChildren mx $ Just xs)) mempty

-- | Existentially quantified case
instance ( Eq k
         , Hashable k
         , Typeable r
         ) => Extend (PathChunk k ('Just r)) (RootedPredTrie k (r -> a)) (RootedPredTrie k a) where
  extend (Pred' i q) (RootedPredTrie mx xs) = RootedPredTrie Nothing $
    PredTrie mempty (PredStep (HM.singleton i (Pred q mx xs)))


-- | @FoldR Extend start chunks ~ result@
class Extrude chunks start result | chunks start -> result where
  extrude :: chunks -> start -> result

-- Basis
instance Extrude (PathChunks k '[]) (RootedPredTrie k a) (RootedPredTrie k a) where
  extrude Nil r = r

-- Successor
instance ( Extrude (PathChunks k xs) trie0 trie1
         , Extend  (PathChunk k x)   trie1 trie2
         ) => Extrude (PathChunks k (x ': xs)) trie0 trie2 where
  extrude (Cons u us) r = extend u $! extrude us r


-- | A simple proof showing that the list version and function version are
--   interchangable.
type ExtrudeSoundly k cleanxs xs c r =
  ( cleanxs ~ CatMaybes xs
  , ArityTypeListIso c cleanxs r
  , Extrude (PathChunks k xs)
      (RootedPredTrie k c)
      (RootedPredTrie k r)
  )

-- * Query Types

-- | Match a literal key
only :: k -> PathChunk k 'Nothing
only = Lit

-- | Match with a predicate against the url chunk directly.
pred :: k -> (k -> Maybe r) -> PathChunk k ('Just r)
pred = Pred'


-- | Constrained to AttoParsec, Regex-Compat and T.Text
data PathChunk k (mx :: Maybe *) where
  Lit   :: { litChunk :: !k
           } -> PathChunk k 'Nothing
  Pred' :: { predTag  :: !k
           , predPred :: !(k -> Maybe r)
           } -> PathChunk k ('Just r)

-- | Use raw strings instead of prepending @l@
instance IsString k => IsString (PathChunk k 'Nothing) where
  fromString = Lit . fromString

-- | Container when defining route paths
data PathChunks k (xs :: [Maybe *]) where
  Cons :: PathChunk k mx
       -> PathChunks k xs
       -> PathChunks k (mx ': xs)
  Nil  :: PathChunks k '[]


-- | The cons-cell for building a query path.
(./) :: PathChunk k mx -> PathChunks k xs -> PathChunks k (mx ': xs)
(./) = Cons

infixr 9 ./

-- | The basis, equivalent to @[]@
nil :: PathChunks k '[]
nil = Nil
