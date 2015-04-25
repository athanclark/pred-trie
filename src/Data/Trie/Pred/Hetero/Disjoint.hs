module Data.Trie.Pred.Hetero.Disjoint
  ( RDPTrie (..)
  , merge
  , lookup
  , lookupNearestParent
  , litSingleton
  , litExtrude
  , module Data.Trie.Pred.Disjoint.Tail
  ) where

import Prelude hiding (lookup)
import Data.Trie.Pred.Disjoint.Tail hiding (lookup, lookupNearestParent, merge)
import qualified Data.Trie.Pred.Disjoint.Tail as ND
import Data.Monoid
import qualified Data.List.NonEmpty as NE


-- | A Rooted, predicate, disjointly indexed trie
data RDPTrie p t x = Rooted (Maybe x) [DPTrie p t x]

instance (Eq p, Eq t) => Monoid (RDPTrie p t x) where
  mempty = Rooted Nothing []
  mappend = Data.Trie.Pred.Disjoint.merge

merge :: (Eq p, Eq t) => RDPTrie p t x -> RDPTrie p t x -> RDPTrie p t x
merge (Rooted mx xs) (Rooted my ys) =
  Rooted my $ foldr go [] $ xs ++ ys
  where
    go :: (Eq p, Eq t) => DPTrie p t x -> [DPTrie p t x] -> [DPTrie p t x]
    go a [] = [a]
    go a (b:bs) | ND.areDisjoint a b =        a : b : bs
                | otherwise          = ND.merge a b : bs

lookup :: (Eq t) => [t] -> RDPTrie p t x -> Maybe x
lookup [] (Rooted mx _) = mx
lookup ts (Rooted _ xs) = firstJust $ map (ND.lookup $ NE.fromList ts) xs

lookupNearestParent :: (Eq t) => [t] -> RDPTrie p t x -> Maybe x
lookupNearestParent [] (Rooted mx _) = mx
lookupNearestParent ts (Rooted mx xs) =
  getFirst $ (First $ firstJust $ map (ND.lookupNearestParent $ NE.fromList ts) xs) <> First mx

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x :xs) = Just x


litSingleton :: [t] -> x -> RDPTrie p t x
litSingleton [] x = Rooted (Just x) []
litSingleton ts x = Rooted Nothing [ND.litSingletonTail (NE.fromList ts) x]


litExtrude :: [t] -> RDPTrie p t x -> RDPTrie p t x
litExtrude [] r = r
litExtrude [t] (Rooted mx xs) = Rooted Nothing [DMore t mx xs]
litExtrude ts (Rooted mx xs) = Rooted Nothing [ND.litExtrudeTail (init ts) $
                                                 DMore (last ts) mx xs
                                              ]

