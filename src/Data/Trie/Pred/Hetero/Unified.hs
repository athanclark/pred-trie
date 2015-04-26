module Data.Trie.Pred.Hetero.Unified
  ( RHUPTrie (..)
  , merge
  , lookup
  -- , lookupNearestParent
  , litSingleton
  , litExtrude
  , module Data.Trie.Pred.Hetero.Unified.Tail
  ) where

import Prelude hiding (lookup)
import Data.Trie.Pred.Hetero.Unified.Tail hiding (lookup, lookupNearestParent, merge)
import qualified Data.Trie.Pred.Hetero.Unified.Tail as NU
import Data.Monoid
import qualified Data.List.NonEmpty as NE
import Control.Applicative


data RHUPTrie t a b = Rooted { root :: Maybe a
                             , children :: [HUPTrie t a b] }

instance (Eq t) => Monoid (RHUPTrie t a b) where
  mempty = Rooted Nothing []
  mappend = Data.Trie.Pred.Hetero.Unified.merge

merge :: (Eq t) => RHUPTrie t a b -> RHUPTrie t a b -> RHUPTrie t a b
merge (Rooted mx xs) (Rooted my ys) =
  Rooted (getLast $ Last mx <> Last my) $ NU.sort $ foldr go [] $ xs ++ ys
  where
    go :: (Eq t) => HUPTrie t a b -> [HUPTrie t a b] -> [HUPTrie t a b]
    go a [] = [a]
    go a (b:bs) | NU.areDisjoint a b =        a : b : bs
                | otherwise          = NU.merge a b : bs

lookup :: (Eq t) => [t] -> RHUPTrie t a b -> Maybe (Either a b)
lookup [] (Rooted mx _) = Left <$> mx
lookup ts (Rooted _ xs) = firstJust $ map (NU.lookup $ NE.fromList ts) xs

-- lookupNearestParent :: (Eq t) => [t] -> RHUPTrie t a b -> Maybe x
-- lookupNearestParent [] (Rooted mx _) = mx
-- lookupNearestParent ts (Rooted mx xs) =
--   getFirst $ (First $ firstJust $ map (NU.lookupNearestParent $ NE.fromList ts) xs) <> First mx

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x :xs) = Just x

litSingleton :: [t] -> a -> RHUPTrie t a b
litSingleton [] x = Rooted (Just x) []
litSingleton ts x = Rooted Nothing [NU.litSingletonTail (NE.fromList ts) x]


litExtrude :: [t] -> RHUPTrie t a b -> RHUPTrie t a b
litExtrude [] r = r
litExtrude [t] (Rooted mx xs) = Rooted Nothing [HUMore t mx xs]
litExtrude ts (Rooted mx xs) = Rooted Nothing [NU.litExtrudeTail (init ts) $
                                                 HUMore (last ts) mx xs
                                              ]
