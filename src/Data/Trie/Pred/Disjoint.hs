module Data.Trie.Pred.Disjoint where

import Data.Trie.Pred.Disjoint.Tail
import qualified Data.Trie.Pred.Disjoint.Tail as ND
import Data.Monoid
import qualified Data.List.NonEmpty as NE


-- | A Rooted, predicate, disjointly indexed trie
data RPDTrie p t x = Rooted (Maybe x) [NDPTrie p t x]

instance (Eq p, Eq t) => Monoid (RPDTrie p t x) where
  mempty = Rooted Nothing []
  mappend = Data.Trie.Pred.Disjoint.merge

merge :: (Eq p, Eq t) => RPDTrie p t x -> RPDTrie p t x -> RPDTrie p t x
merge (Rooted mx xs) (Rooted my ys) =
  Rooted my $ foldr go [] $ xs ++ ys
  where
    go :: (Eq p, Eq t) => NDPTrie p t x -> [NDPTrie p t x] -> [NDPTrie p t x]
    go a [] = [a]
    go a (b:bs) | ND.areDisjoint a b =          a : b : bs
                | otherwise          = (ND.merge a b) : bs

lookup :: (Eq t) => [t] -> RPDTrie p t x -> Maybe x
lookup [] (Rooted mx _) = mx
lookup ts (Rooted _ xs) = getFirst $ map (ND.lookup $ NE.fromList ts) xs
  where
    getFirst :: [Maybe a] -> Maybe a
    getFirst [] = Nothing
    getFirst (Nothing:xs) = getFirst xs
    getFirst (Just x :xs) = Just x
