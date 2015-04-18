{-# LANGUAGE
  GADTs
  #-}

module Data.Trie.Pred
  ( PredTrie (..)
  , lookup
  ) where

import Data.Trie.Pred.Internal

import Prelude hiding (lookup)
import Data.List.NonEmpty hiding (map)
import Data.List.NonEmpty as NE hiding (map)


data PredTrie t x where
  Rest :: NonEmpty t ->           x ->                                   PredTrie t x
  More ::  t ->             Maybe x ->        NonEmpty (PredTrie t x) -> PredTrie t x
  Pred :: (t -> Maybe r) -> Maybe (r -> x) -> [PredTrie t (r -> x)] ->   PredTrie t x

lookup :: Eq t => NonEmpty t -> PredTrie t x -> Maybe x
lookup tss@(t:|ts) (Rest ps x) | tss == ps = Just x
                               | otherwise = Nothing
lookup     (t:|ts) (More t' mx xs) | t == t' =
  case ts of
    [] -> mx
    _  -> getFirst $ NE.toList $ fmap (lookup $ NE.fromList ts) xs
                                   | otherwise = Nothing
lookup     (t:|ts) (Pred p mrx xrs) =
  p t >>=
    \r -> case ts of
      [] -> ($ r) <$> mrx
      _  -> ($ r) <$> (getFirst $ map (lookup $ NE.fromList ts) xrs)


getFirst :: [Maybe a] -> Maybe a
getFirst [] = Nothing
getFirst (Nothing:xs) = getFirst xs
getFirst (Just x :xs) = Just x
