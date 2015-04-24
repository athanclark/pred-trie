module Data.Trie.Pred.Unified
  ( RUPTrie (..)
  , merge
  , lookup
  , lookupNearestParent
  , litSingleton
  , litExtrude
  ) where

import Prelude hiding (lookup)
import Data.Trie.Pred.Unified.Tail hiding (lookup, lookupNearestParent, merge)
import qualified Data.Trie.Pred.Unified.Tail as NU
import Data.Monoid
import qualified Data.List.NonEmpty as NE


data RUPTrie t x = Rooted { root :: Maybe x
                          , children :: [UPTrie t x] }

instance (Eq t) => Monoid (RUPTrie t x) where
  mempty = Rooted Nothing []
  mappend = Data.Trie.Pred.Unified.merge

merge :: (Eq t) => RUPTrie t x -> RUPTrie t x -> RUPTrie t x
merge (Rooted mx xs) (Rooted my ys) =
  Rooted (getLast $ Last mx <> Last my) $ NU.sort $ foldr go [] $ xs ++ ys
  where
    go :: (Eq t) => UPTrie t x -> [UPTrie t x] -> [UPTrie t x]
    go a [] = [a]
    go a (b:bs) | NU.areDisjoint a b =          a : b : bs
                | otherwise          = (NU.merge a b) : bs

lookup :: (Eq t) => [t] -> RUPTrie t x -> Maybe x
lookup [] (Rooted mx _) = mx
lookup ts (Rooted _ xs) = firstJust $ map (NU.lookup $ NE.fromList ts) xs

lookupNearestParent :: (Eq t) => [t] -> RUPTrie t x -> Maybe x
lookupNearestParent [] (Rooted mx _) = mx
lookupNearestParent ts (Rooted mx xs) =
  getFirst $ (First $ firstJust $ map (NU.lookupNearestParent $ NE.fromList ts) xs) <> (First mx)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x :xs) = Just x

litSingleton :: [t] -> x -> RUPTrie t x
litSingleton [] x = Rooted (Just x) []
litSingleton ts x = Rooted Nothing [NU.litSingletonTail (NE.fromList ts) x]


litExtrude :: [t] -> RUPTrie t x -> RUPTrie t x
litExtrude [] r = r
litExtrude (t:[]) (Rooted mx xs) = Rooted Nothing [UMore t mx xs]
litExtrude ts (Rooted mx xs) = Rooted Nothing [NU.litExtrudeTail (init ts) $
                                                 UMore (last ts) mx xs
                                              ]
