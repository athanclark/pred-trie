{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , DeriveFunctor
  #-}

module Data.Trie.Pred.Types where

import Prelude hiding (lookup)
import Data.Trie.Class
import qualified Data.Trie.Map as MT
import qualified Data.Map as M
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Functor.Syntax
import Data.Monoid


-- * Single Predicated Step

data PredStep c s a = forall r. PredStep
  { predTag  :: s -- ^ Unique identifier for the predicate - used for combination
  , predPred :: s -> Maybe r
  , predData :: Maybe (r -> a)
  , predSub  :: c s (r -> a)
  }

instance Functor (c s) => Functor (PredStep c s) where
  fmap f (PredStep i p mx xs) = PredStep i p (f <.$> mx) $ f <.$> xs

-- | Lookup and delete only - can't arbitrarilly construct a predicated trie.
instance Trie NonEmpty s c => Trie NonEmpty s (PredStep c) where
  lookup (t:|ts) (PredStep _ p mx xs) = do
    r <- p t
    if null ts then mx <$~> r
               else lookup (NE.fromList ts) xs <$~> r
  delete (t:|ts) xss@(PredStep i p mx xs) =
    maybe xss
      (const $ if null ts
               then PredStep i p Nothing xs
               else PredStep i p mx $ delete (NE.fromList ts) xs)
      (p t)

singletonPred :: Monoid (c s (r -> a)) => s -> (s -> Maybe r) -> (r -> a) -> PredStep c s a
singletonPred i p x = PredStep i p (Just x) mempty


-- * Adjacent Predicated Steps

newtype PredSteps c s a = PredSteps
  { unPredSteps :: [PredStep c s a] }
  deriving (Functor)

  -- | Lookup and delete only - can't arbitrarilly construct a predicated trie.
instance Trie NonEmpty s c => Trie NonEmpty s (PredSteps c) where
  lookup ts (PredSteps ps) = getFirst $ foldMap (First . lookup ts) ps
  delete ts (PredSteps ps) = PredSteps $ fmap (delete ts) ps

instance Eq s => Monoid (PredSteps c s a) where
  mempty = PredSteps []
  mappend = unionPred

unionPred :: Eq s => PredSteps c s a -> PredSteps c s a -> PredSteps c s a
unionPred (PredSteps (xss@(PredStep i p mx xs):pxs)) (PredSteps (yss@(PredStep j q my ys):pys))
  | i == j = PredSteps $ yss : unPredSteps (unionPred (PredSteps pxs) (PredSteps pys))
  | otherwise = PredSteps $ xss : yss : unPredSteps (unionPred (PredSteps pxs) (PredSteps pys))
unionPred x (PredSteps []) = x
unionPred (PredSteps []) y = y


-- * Predicated Trie

data PredTrie s a = PredTrie
  { predLits  :: MT.MapStep PredTrie s a
  , predPreds :: PredSteps PredTrie s a
  } deriving (Functor)

instance Ord s => Trie NonEmpty s PredTrie where
  lookup ts (PredTrie ls ps) =
    getFirst $ First (lookup ts ls) <> First (lookup ts ps)
  delete ts (PredTrie ls ps) = PredTrie (delete ts ls) (delete ts ps)
  insert ts x (PredTrie ls ps) = PredTrie (insert ts x ls) ps -- can only insert literals

instance Ord s => Monoid (PredTrie s a) where
  mempty = PredTrie mempty mempty
  mappend (PredTrie ls1 ps1) (PredTrie ls2 ps2) =
    PredTrie (ls1 <> ls2) (ps1 <> ps2)

empty :: PredTrie s a
empty = PredTrie MT.empty (PredSteps [])

-- subtrie :: Ord s => NonEmpty s -> PredTrie s a -> PredTrie s a
-- subtrie (t:|ts) (PredTrie (MapTrie (MapStep ls)) ps)
--   | null ts = getFirst $ First (lookup ts ls)
