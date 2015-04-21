{-# LANGUAGE
  GADTs
  #-}

module Data.Trie.Pred.Disjoint.Tail
  ( DPTrie (..)
  , lookup
  , merge
  , areDisjoint
  , litSingletonTail
  , litExtrudeTail
  ) where

import Prelude hiding (lookup)
import Data.List.NonEmpty hiding (map)
import Data.List.NonEmpty as NE hiding (map)



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
    _  -> getFirst $ map (lookup $ NE.fromList ts) xs
  | otherwise = Nothing
lookup (t:|ts) (DPred _ p mrx xrs) =
  p t >>=
    \r -> case ts of
      [] -> ($ r) <$> mrx
      _  -> ($ r) <$> (getFirst $ map (lookup $ NE.fromList ts) xrs)


getFirst :: [Maybe a] -> Maybe a
getFirst [] = Nothing
getFirst (Nothing:xs) = getFirst xs
getFirst (Just x :xs) = Just x


litSingletonTail :: NonEmpty t -> x -> DPTrie p t x
litSingletonTail (t:|[]) x = DMore t (Just x) []
litSingletonTail (t:|ts) x = DMore t Nothing  [buildLitSingletonTail (NE.fromList ts) x]


litExtrudeTail :: [t] -> DPTrie t x -> DPTrie t x
litExtrudeTail [] r = r
litExtrudeTail (t:ts) r = DMore t Nothing [litExtrudeTail ts r]

