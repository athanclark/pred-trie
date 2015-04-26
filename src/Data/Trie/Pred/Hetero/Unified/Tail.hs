{-# LANGUAGE
  GADTs
  #-}

module Data.Trie.Pred.Hetero.Unified.Tail
  ( HUPTrie (..)
  , lookup
  --, lookupNearestParent
  , merge
  , areDisjoint
  , litSingletonTail
  , litExtrudeTail
  , sort
  ) where

import Prelude hiding (lookup)
import Data.List.NonEmpty as NE hiding (map, sort)
import Control.Applicative


data HUPTrie t a b where
  HUMore :: t
         -> Maybe a
         -> [HUPTrie t a b]
         -> HUPTrie t a b
  HUPred :: t
         -> (t -> Maybe r)
         -> Maybe (r -> b)
         -> [HUPTrie t (r -> a) (r -> b)]
         -> HUPTrie t a b


-- | Overwrites when similar, leaves untouched when not
merge :: (Eq t) => HUPTrie t a b -> HUPTrie t a b -> HUPTrie t a b
merge xx@(HUMore t mx xs) yy@(HUMore p my ys)
  | t == p = HUMore p my $ sort $ xs ++ ys
  | otherwise = xx
merge xx@(HUPred t q mrx xrs) yy@(HUPred p w mry yrs)
  | t == p = yy -- predicate children are incompatible
  | otherwise = xx
merge xx@(HUMore t mx xs) yy@(HUPred p w mrx xrs)
  | t == p = yy -- rightward bias
  | otherwise = xx
merge xx@(HUPred t q mrx xrs) yy@(HUMore p my ys)
  | t == p = yy -- rightward bias
  | otherwise = xx


areDisjoint :: (Eq t) => HUPTrie t a b -> HUPTrie t a b -> Bool
areDisjoint (HUMore t _ _)    (HUMore p _ _)    = t /= p
areDisjoint (HUPred t _ _ _)  (HUPred p _ _ _)  = t /= p
areDisjoint (HUPred t _ _ _)  (HUMore p _ _)    = t /= p
areDisjoint (HUMore t _ _)    (HUPred p _ _ _)  = t /= p


lookup :: Eq t => NonEmpty t -> HUPTrie t a b -> Maybe (Either a b)
lookup (t:|ts) (HUMore t' mx xs)
  | t == t' = case ts of
    [] -> Left <$> mx
    _  -> firstJust $ map (lookup $ NE.fromList ts) xs
  | otherwise = Nothing
lookup (t:|ts) (HUPred _ p mrx xrs) =
  p t >>=
    \r -> case ts of
      [] -> Right <$> ($ r) <$> mrx
      _  -> case firstJust (map (lookup $ NE.fromList ts) xrs) of
              Nothing -> Nothing
              Just es -> Just $ appEither es r
  where
    appEither :: Either (r -> a) (r -> b) -> r -> Either a b
    appEither (Left f) r = Left $ f r
    appEither (Right g) r = Right $ g r


-- lookupNearestParent :: Eq t => NonEmpty t -> UPTrie t x -> Maybe x
-- lookupNearestParent tss@(t:|ts) trie@(UMore t' mx xs) = case lookup tss trie of
--   Nothing -> if t == t'
--                then case ts of
--                       [] -> mx -- redundant; should have successful lookup
--                       _  -> case firstJust $ map (lookupNearestParent $ NE.fromList ts) xs of
--                               Nothing -> mx
--                               justr   -> justr
--                else Nothing
--   justr -> justr
-- lookupNearestParent tss@(t:|ts) trie@(UPred t' p mrx xrs) = case lookup tss trie of
--   Nothing -> p t >>=
--                \r -> case ts of
--                         [] -> ($ r) <$> mrx -- redundant; should have successful lookup
--                         _  -> case firstJust $ map (lookupNearestParent $ NE.fromList ts) xrs of
--                                 Nothing -> ($ r) <$> mrx
--                                 justr   -> ($ r) <$> justr
--   justr -> justr



firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x :xs) = Just x


litSingletonTail :: NonEmpty t -> a -> HUPTrie t a b
litSingletonTail (t:|[]) x = HUMore t (Just x) []
litSingletonTail (t:|ts) x = HUMore t Nothing  [litSingletonTail (NE.fromList ts) x]


litExtrudeTail :: [t] -> HUPTrie t a b -> HUPTrie t a b
litExtrudeTail [] r = r
litExtrudeTail (t:ts) r = HUMore t Nothing [litExtrudeTail ts r]


-- also does a non-deterministic merge - make sure your nodes are disjoint & clean
sort :: (Eq t) => [HUPTrie t a b] -> [HUPTrie t a b]
sort = foldr insert []
  where
    insert :: (Eq t) => HUPTrie t a b -> [HUPTrie t a b] -> [HUPTrie t a b]
    insert r [] = [r]
    insert x@(HUMore t _ _) (y@(HUMore p _ _):rs)
      | t == p = x : rs
      | otherwise = x : y : rs
    insert x@(HUMore t _ _) (y@(HUPred p _ _ _):rs)
      | t == p = x : rs
      | otherwise = x : y : rs
    insert x@(HUPred t _ _ _) (y@(HUPred p _ _ _):rs)
      | t == p = x : rs -- basis
      | otherwise = x : y : rs
    insert x@(HUPred t _ _ _) (y@(HUMore p _ _):rs)
      | t == p = insert x rs
      | otherwise = y : insert x rs

