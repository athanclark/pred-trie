module Data.Trie.Pred.Unified
  ( RUPTrie (..)
  , UPTrie (..)
  , assignLit
  , showTrie
  , merge
  , elem
  , lookup
  , lookupWithL
  , lookupNearestParent
  , lookupThrough
  , litSingleton
  , litExtrude
  ) where

import Prelude hiding (lookup, map, elem)
import           Data.Trie.Pred.Unified.Tail (UPTrie (..), showTail)
import qualified Data.Trie.Pred.Unified.Tail as NU
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import Data.Maybe
import Data.Functor.Syntax

import Test.QuickCheck


data RUPTrie t x = Rooted { root :: Maybe x
                          , children :: [UPTrie t x] }
  deriving (Eq)

instance Functor (RUPTrie t) where
  fmap = map

map :: (a -> b) -> RUPTrie t a -> RUPTrie t b
map f (Rooted mx xs) = Rooted (f <$> mx) $ f <$$> xs

instance Foldable (RUPTrie t) where
  foldMap f (Rooted mx xs) = fromMaybe (foldMap (foldMap f) xs) $ f <$> mx

showTrie :: Show t => RUPTrie t x -> String
showTrie (Rooted mx xs) =
  if isNothing mx
  then "(NoRoot) [" ++ concatMap showTail xs ++ "] "
  else "(Root) [" ++ concatMap showTail xs ++ "] "

instance (Eq t) => Monoid (RUPTrie t x) where
  mempty = Rooted Nothing []
  mappend = Data.Trie.Pred.Unified.merge

merge :: (Eq t) => RUPTrie t x -> RUPTrie t x -> RUPTrie t x
merge (Rooted mx xs) (Rooted my ys) =
  Rooted (getLast $ Last mx <> Last my) $ NU.sort $ foldr go [] $ xs ++ ys
  where
    go :: (Eq t) => UPTrie t x -> [UPTrie t x] -> [UPTrie t x]
    go a [] = [a]
    go a (b:bs) | NU.areDisjoint a b =        a : b : bs
                | otherwise          = NU.merge a b : bs

instance (Show t) => Show (RUPTrie t x) where
  show = showTrie

instance (Arbitrary t, Arbitrary x) => Arbitrary (RUPTrie t x) where
  arbitrary = do
    mx <- arbitrary
    xs <- arbitrary `suchThat` (\x -> length x < 10)
    return $ Rooted mx xs


assignLit :: Eq t => [t] -> Maybe x -> RUPTrie t x -> RUPTrie t x
assignLit [] mx (Rooted _ ys) = Rooted mx ys
assignLit ts mx (Rooted my ys) = Rooted my $
  NU.assignLit (NE.fromList ts) mx <$> ys

elem :: (Eq t) => [t] -> RUPTrie t x -> Bool
elem ts = isJust . lookup ts

lookup :: (Eq t) => [t] -> RUPTrie t x -> Maybe x
lookup [] (Rooted mx _) = mx
lookup ts (Rooted _ xs) = firstJust $ NU.lookup (NE.fromList ts) <$> xs

-- | Applies @f@ to the last chunk.
lookupWithL :: (Eq t) => (t -> t) -> [t] -> RUPTrie t x -> Maybe x
lookupWithL _ [] (Rooted mx _) = mx
lookupWithL f ts (Rooted _ xs) = firstJust $ NU.lookupWithL f (NE.fromList ts) <$> xs

lookupNearestParent :: (Eq t) => [t] -> RUPTrie t x -> Maybe x
lookupNearestParent [] (Rooted mx _) = mx
lookupNearestParent ts (Rooted mx xs) =
  firstJust $ (NU.lookupNearestParent (NE.fromList ts) <$> xs) ++ [mx]

-- | Append contents up-to lookup path.
lookupThrough :: (Eq t, Monoid x) => [t] -> RUPTrie t x -> [x]
lookupThrough [] (Rooted mx _) = maybeToList mx
lookupThrough ts (Rooted mx xs) =
  maybeToList mx ++ NU.firstNonEmpty (NU.lookupThrough (NE.fromList ts) <$> xs)

litSingleton :: [t] -> x -> RUPTrie t x
litSingleton [] x = Rooted (Just x) []
litSingleton ts x = Rooted Nothing [NU.litSingletonTail (NE.fromList ts) x]


litExtrude :: [t] -> RUPTrie t x -> RUPTrie t x
litExtrude [] r = r
litExtrude [t] (Rooted mx xs) = Rooted Nothing [UMore t mx xs]
litExtrude ts (Rooted mx xs) = Rooted Nothing [NU.litExtrudeTail (init ts) $
                                                 UMore (last ts) mx xs
                                              ]

-- * Utilities

firstJust :: [Maybe a] -> Maybe a
firstJust = getFirst . foldMap First
