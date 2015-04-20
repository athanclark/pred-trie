module Data.Trie.Pred.Unified where

import Data.Trie.Pred.Unified.Tail
import qualified Data.Trie.Pred.Unified.Tail as NU
import Data.Monoid
import qualified Data.List.NonEmpty as NE


data RPUTrie t x = Rooted (Maybe x) [NUPTrie t x]

instance (Eq t) => Monoid (RPUTrie t x) where
  mempty = Rooted Nothing []
  mappend = Data.Trie.Pred.Unified.merge

merge :: (Eq t) => RPUTrie t x -> RPUTrie t x -> RPUTrie t x
merge (Rooted mx xs) (Rooted my ys) =
  Rooted my $ foldr go [] $ xs ++ ys
  where
    go :: (Eq t) => NUPTrie t x -> [NUPTrie t x] -> [NUPTrie t x]
    go a [] = [a]
    go a (b:bs) | NU.areDisjoint a b =          a : b : bs
                | otherwise          = (NU.merge a b) : bs

lookup :: (Eq t) => [t] -> RPUTrie t x -> Maybe x
lookup [] (Rooted mx _) = mx
lookup ts (Rooted _ xs) = getFirst $ map (NU.lookup $ NE.fromList ts) xs
  where
    getFirst :: [Maybe a] -> Maybe a
    getFirst [] = Nothing
    getFirst (Nothing:xs) = getFirst xs
    getFirst (Just x :xs) = Just x
