{-# LANGUAGE
  GADTs
  #-}

module Data.Trie.Pred
  ( PredTrie (..)
  , lookup
  , merge
  , areDisjoint
  ) where

import Data.Trie.Pred.Internal

import Prelude hiding (lookup)
import Data.List.NonEmpty hiding (map)
import Data.List.NonEmpty as NE hiding (map)


-- | A predicative trie is composed of explicit predicate labels (for equality
-- tests of predicates), a type for node labels, and some content type
data PredTrie p t x where
  Rest :: NonEmpty t
       -> x
       -> PredTrie p t x
  More :: t
       -> Maybe x
       -> NonEmpty (PredTrie p t x)
       -> PredTrie p t x
  Pred :: p
       -> (t -> Maybe r)
       -> Maybe (r -> x)
       -> [PredTrie p t (r -> x)]
       -> PredTrie p t x


-- | Overwrites when similar, leaves untouched when not
merge :: (Eq t, Eq p) => PredTrie p t x -> PredTrie p t x -> PredTrie p t x
merge xx@(Rest tss@(t:|ts) x) yy@(Rest pss@(p:|ps) y)
  | tss == pss = yy
  | t == p     = let
                   xx' = Rest (NE.fromList ts) x
                   yy' = Rest (NE.fromList ps) y
                 in
                 More p Nothing $
                   if areDisjoint xx' yy'
                     then NE.fromList [xx', yy']
                     else NE.fromList
                            [merge (Rest (NE.fromList ts) x) (Rest (NE.fromList ps) y)]
  | otherwise = xx
merge xx@(More t mx xs) yy@(More p my ys)
  | t == p = More p my $ NE.fromList $ foldr go [] $ (NE.toList xs) ++ (NE.toList ys)
  | otherwise = xx
  where
    go :: (Eq t, Eq p) => PredTrie p t x -> [PredTrie p t x] -> [PredTrie p t x]
    go a [] = [a]
    go a (b:bs) | areDisjoint a b =       a : b : bs
                | otherwise       = (merge a b) : bs
merge xx@(Pred t q mrx xrs) yy@(Pred p w mry yrs)
  | t == p = yy
  | otherwise = xx
merge xx@(Rest (t:|ts) x) yy@(More p my ys)
  | t == p = case ts of
               [] -> More p (Just x) ys
               _  -> More p my $ fmap (merge $ Rest (NE.fromList ts) x) ys
  | otherwise = xx
merge xx@(More t mx xs) yy@(Rest (p:|ps) y)
  | t == p = case ps of
               [] -> More t (Just y) xs
               _  -> More t mx $ fmap (flip merge $ Rest (NE.fromList ps) y) xs
  | otherwise = yy
merge xx yy@(Pred _ _ _ _) = yy -- Predicates are more general
merge xx@(Pred _ _ _ _) yy = xx


areDisjoint :: (Eq t, Eq p) => PredTrie p t x -> PredTrie p t x -> Bool
areDisjoint (Rest (t:|_) _) (Rest (p:|_) _) = t == p
areDisjoint (More t _ _)    (More p _ _)    = t == p
areDisjoint (Pred t _ _ _)  (Pred p _ _ _)  = t == p


lookup :: Eq t => NonEmpty t -> PredTrie p t x -> Maybe x
lookup tss@(t:|ts) (Rest ps x) | tss == ps = Just x
                               | otherwise = Nothing
lookup     (t:|ts) (More t' mx xs) | t == t' =
  case ts of
    [] -> mx
    _  -> getFirst $ NE.toList $ fmap (lookup $ NE.fromList ts) xs
                                   | otherwise = Nothing
lookup     (t:|ts) (Pred _ p mrx xrs) =
  p t >>=
    \r -> case ts of
      [] -> ($ r) <$> mrx
      _  -> ($ r) <$> (getFirst $ map (lookup $ NE.fromList ts) xrs)


getFirst :: [Maybe a] -> Maybe a
getFirst [] = Nothing
getFirst (Nothing:xs) = getFirst xs
getFirst (Just x :xs) = Just x
