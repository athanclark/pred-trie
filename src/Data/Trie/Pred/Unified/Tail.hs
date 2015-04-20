{-# LANGUAGE
  GADTs
  #-}

module Data.Trie.Pred.Unified.Tail
  ( UPTrie (..)
  , lookup
  , merge
  , areDisjoint
  ) where

import Prelude hiding (lookup)
import Data.List.NonEmpty hiding (map)
import Data.List.NonEmpty as NE hiding (map)



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
  | t == p = UMore p my $ foldr go [] $ xs ++ ys
  | otherwise = xx
  where
    go :: (Eq t) => UPTrie t x -> [UPTrie t x] -> [UPTrie t x]
    go a [] = [a]
    go a (b:bs) | areDisjoint a b =       a : b : bs
                | otherwise       = (merge a b) : bs
merge xx@(UPred t q mrx xrs) yy@(UPred p w mry yrs)
  | t == p = yy
  | otherwise = xx
merge xx@(UMore t mx xs) yy@(UPred p w mrx xrs)
  | t == p = yy -- predicate children are incompatible
  | otherwise = xx
merge xx@(UPred t q mrx xrs) yy@(UMore p my ys)
  | t == p = yy
  | otherwise = xx


areDisjoint :: (Eq t) => UPTrie t x -> UPTrie t x -> Bool
areDisjoint (UMore t _ _)    (UMore p _ _)    = t == p
areDisjoint (UPred t _ _ _)  (UPred p _ _ _)  = t == p
areDisjoint (UPred t _ _ _)  (UMore p _ _)    = t == p
areDisjoint (UMore t _ _)    (UPred p _ _ _)  = t == p


lookup :: Eq t => NonEmpty t -> UPTrie t x -> Maybe x
lookup (t:|ts) (UMore t' mx xs)
  | t == t' = case ts of
    [] -> mx
    _  -> getFirst $ map (lookup $ NE.fromList ts) xs
  | otherwise = Nothing
lookup (t:|ts) (UPred _ p mrx xrs) =
  p t >>=
    \r -> case ts of
      [] -> ($ r) <$> mrx
      _  -> ($ r) <$> (getFirst $ map (lookup $ NE.fromList ts) xrs)


getFirst :: [Maybe a] -> Maybe a
getFirst [] = Nothing
getFirst (Nothing:xs) = getFirst xs
getFirst (Just x :xs) = Just x


buildLitSingletonTail :: NonEmpty t -> x -> UPTrie t x
buildLitSingletonTail (t:|[]) x = UMore t (Just x) []
buildLitSingletonTail (t:|ts) x = UMore t Nothing  [buildLitSingletonTail (NE.fromList ts) x]

