{-# LANGUAGE
  GADTs
  #-}

module Data.Trie.Pred.Disjoint.Tail
  ( DPTrie (..)
  , lookup
  , lookupNearestParent
  , merge
  , areDisjoint
  , litSingletonTail
  , litExtrudeTail
  , sort
  ) where

import Prelude hiding (lookup)
import Data.List.NonEmpty hiding (map, sort)
import Data.List.NonEmpty as NE hiding (map, sort)



data DPTrie p t x where
  DMore :: t
        -> Maybe x
        -> [DPTrie p t x]
        -> DPTrie p t x
  DPred :: p
        -> (t -> Maybe r)
        -> Maybe (r -> x)
        -> [DPTrie p t (r -> x)]
        -> DPTrie p t x


-- | Overwrites when similar, leaves untouched when not
merge :: (Eq p, Eq t) => DPTrie p t x -> DPTrie p t x -> DPTrie p t x
merge xx@(DMore t mx xs) yy@(DMore p my ys)
  | t == p = DMore p my $ foldr go [] $ xs ++ ys
  | otherwise = xx
  where
    go :: (Eq p, Eq t) => DPTrie p t x -> [DPTrie p t x] -> [DPTrie p t x]
    go a [] = [a]
    go a (b:bs) | areDisjoint a b =       a : b : bs
                | otherwise       = (merge a b) : bs
merge xx@(DPred t q mrx xrs) yy@(DPred p w mry yrs)
  | t == p = yy
  | otherwise = xx
merge xx@(DMore t mx xs) yy@(DPred p w mrx xrs) = yy
merge xx@(DPred t q mrx xrs) yy@(DMore p my ys) = yy


areDisjoint :: (Eq p, Eq t) => DPTrie p t x -> DPTrie p t x -> Bool
areDisjoint (DMore t _ _)    (DMore p _ _)    = t == p
areDisjoint (DPred t _ _ _)  (DPred p _ _ _)  = t == p
areDisjoint _ _ = True


lookup :: Eq t => NonEmpty t -> DPTrie p t x -> Maybe x
lookup (t:|ts) (DMore t' mx xs)
  | t == t' = case ts of
    [] -> mx
    _  -> firstJust $ map (lookup $ NE.fromList ts) xs
  | otherwise = Nothing
lookup (t:|ts) (DPred _ p mrx xrs) =
  p t >>=
    \r -> case ts of
      [] -> ($ r) <$> mrx
      _  -> ($ r) <$> (firstJust $ map (lookup $ NE.fromList ts) xrs)


lookupNearestParent :: Eq t => NonEmpty t -> DPTrie p t x -> Maybe x
lookupNearestParent tss@(t:|ts) trie@(DMore t' mx xs) = case lookup tss trie of
  Nothing -> if t == t'
               then case ts of
                      [] -> mx -- redundant; should have successful lookup
                      _  -> case firstJust $ map (lookupNearestParent $ NE.fromList ts) xs of
                              Nothing -> mx
                              justr   -> justr
               else Nothing
  justr -> justr
lookupNearestParent tss@(t:|ts) trie@(DPred t' p mrx xrs) = case lookup tss trie of
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


litSingletonTail :: NonEmpty t -> x -> DPTrie p t x
litSingletonTail (t:|[]) x = DMore t (Just x) []
litSingletonTail (t:|ts) x = DMore t Nothing  [litSingletonTail (NE.fromList ts) x]


litExtrudeTail :: [t] -> DPTrie p t x -> DPTrie p t x
litExtrudeTail [] r = r
litExtrudeTail (t:ts) r = DMore t Nothing [litExtrudeTail ts r]


sort :: (Eq p, Eq t) => [DPTrie p t x] -> [DPTrie p t x]
sort = foldr insert []
  where
    insert :: (Eq p, Eq t) => DPTrie p t x -> [DPTrie p t x] -> [DPTrie p t x]
    insert r [] = [r]
    insert x@(DMore t _ _) (y@(DMore p _ _):rs)
      | t == p = x : rs
      | otherwise = x : y : rs
    insert x@(DMore t _ _) (y@(DPred p _ _ _):rs) =
        x : y : rs
    insert x@(DPred t _ _ _) (y@(DPred p _ _ _):rs)
      | t == p = x : rs -- basis
      | otherwise = x : y : rs
    insert x@(DPred t _ _ _) (y@(DMore p _ _):rs) =
        y : insert x rs
