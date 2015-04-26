{-# LANGUAGE
  GADTs
  #-}

module Data.Trie.Pred.Unified.Tail
  ( UPTrie (..)
  , lookup
  , lookupNearestParent
  , merge
  , areDisjoint
  , litSingletonTail
  , litExtrudeTail
  , sort
  ) where

import Prelude hiding (lookup)
import Data.List.NonEmpty as NE hiding (map, sort)
import Control.Applicative



data UPTrie t x where
  UMore :: t
        -> Maybe x
        -> [UPTrie t x]
        -> UPTrie t x
  UPred :: t
        -> (t -> Maybe r)
        -> Maybe (r -> x)
        -> [UPTrie t (r -> x)]
        -> UPTrie t x


-- | Overwrites when similar, leaves untouched when not
merge :: (Eq t) => UPTrie t x -> UPTrie t x -> UPTrie t x
merge xx@(UMore t mx xs) yy@(UMore p my ys)
  | t == p = UMore p my $ sort $ xs ++ ys
  | otherwise = xx
merge xx@(UPred t q mrx xrs) yy@(UPred p w mry yrs)
  | t == p = yy -- predicate children are incompatible
  | otherwise = xx
merge xx@(UMore t mx xs) yy@(UPred p w mrx xrs)
  | t == p = yy -- rightward bias
  | otherwise = xx
merge xx@(UPred t q mrx xrs) yy@(UMore p my ys)
  | t == p = yy -- rightward bias
  | otherwise = xx


areDisjoint :: (Eq t) => UPTrie t x -> UPTrie t x -> Bool
areDisjoint (UMore t _ _)    (UMore p _ _)    = t /= p
areDisjoint (UPred t _ _ _)  (UPred p _ _ _)  = t /= p
areDisjoint (UPred t _ _ _)  (UMore p _ _)    = t /= p
areDisjoint (UMore t _ _)    (UPred p _ _ _)  = t /= p


lookup :: Eq t => NonEmpty t -> UPTrie t x -> Maybe x
lookup (t:|ts) (UMore t' mx xs)
  | t == t' = case ts of
    [] -> mx
    _  -> firstJust $ map (lookup $ NE.fromList ts) xs
  | otherwise = Nothing
lookup (t:|ts) (UPred _ p mrx xrs) =
  p t >>=
    \r -> case ts of
      [] -> ($ r) <$> mrx
      _  -> ($ r) <$> firstJust (map (lookup $ NE.fromList ts) xrs)


lookupNearestParent :: Eq t => NonEmpty t -> UPTrie t x -> Maybe x
lookupNearestParent tss@(t:|ts) trie@(UMore t' mx xs) = case lookup tss trie of
  Nothing -> if t == t'
               then case ts of
                      [] -> mx -- redundant; should have successful lookup
                      _  -> case firstJust $ map (lookupNearestParent $ NE.fromList ts) xs of
                              Nothing -> mx
                              justr   -> justr
               else Nothing
  justr -> justr
lookupNearestParent tss@(t:|ts) trie@(UPred t' p mrx xrs) = case lookup tss trie of
  Nothing -> p t >>=
               \r -> case ts of
                        [] -> ($ r) <$> mrx -- redundant; should have successful lookup
                        _  -> case firstJust $ map (lookupNearestParent $ NE.fromList ts) xrs of
                                Nothing -> ($ r) <$> mrx
                                justr   -> ($ r) <$> justr
  justr -> justr



firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x :xs) = Just x


litSingletonTail :: NonEmpty t -> x -> UPTrie t x
litSingletonTail (t:|[]) x = UMore t (Just x) []
litSingletonTail (t:|ts) x = UMore t Nothing  [litSingletonTail (NE.fromList ts) x]


litExtrudeTail :: [t] -> UPTrie t x -> UPTrie t x
litExtrudeTail [] r = r
litExtrudeTail (t:ts) r = UMore t Nothing [litExtrudeTail ts r]


-- also does a non-deterministic merge - make sure your nodes are disjoint & clean
sort :: (Eq t) => [UPTrie t x] -> [UPTrie t x]
sort = foldr insert []
  where
    insert :: (Eq t) => UPTrie t x -> [UPTrie t x] -> [UPTrie t x]
    insert r [] = [r]
    insert x@(UMore t _ _) (y@(UMore p _ _):rs)
      | t == p = x : rs
      | otherwise = x : y : rs
    insert x@(UMore t _ _) (y@(UPred p _ _ _):rs)
      | t == p = x : rs
      | otherwise = x : y : rs
    insert x@(UPred t _ _ _) (y@(UPred p _ _ _):rs)
      | t == p = x : rs -- basis
      | otherwise = x : y : rs
    insert x@(UPred t _ _ _) (y@(UMore p _ _):rs)
      | t == p = insert x rs
      | otherwise = y : insert x rs

