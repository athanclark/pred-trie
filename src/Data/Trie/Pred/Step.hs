{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , DeriveFunctor
  , DeriveGeneric
  , DeriveDataTypeable
  , BangPatterns
  #-}

{- |
Module      : Data.Trie.Pred
Copyright   : (c) 2015 Athan Clark

License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC
-}

module Data.Trie.Pred.Step where

import Prelude hiding (lookup)
import Data.Trie.Class
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Typeable
import Data.Functor.Syntax
import Data.Monoid


-- * Single Predicated Step

data PredStep c s a = forall r. PredStep
  { predTag  :: !s             -- ^ Unique identifier for the predicate - used for combination
  , predPred :: s -> Maybe r   -- ^ The predicate, existentially quantified in the successful result @r@
  , predData :: Maybe (r -> a) -- ^ The result function, capturing the quantified result @r@ and turning
                               --   it into a top-level variable @a@.
  , predSub  :: c s (r -> a)   -- ^ Any sub-trie must have __all__ results preceeded in arity with
                               --   the result at this step.
  } deriving (Typeable)

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

-- | Adjacent steps
newtype PredSteps c s a = PredSteps
  { unPredSteps :: [PredStep c s a]
  } deriving (Functor, Typeable)

-- | Lookup and delete only - can't arbitrarilly construct a predicated trie.
instance Trie NonEmpty s c => Trie NonEmpty s (PredSteps c) where
  lookup ts (PredSteps ps) = getFirst $ foldMap (First . lookup ts) ps
  delete ts (PredSteps ps) = PredSteps $ fmap (delete ts) ps

instance Eq s => Monoid (PredSteps c s a) where
  mempty  = PredSteps []
  mappend = unionPred

-- | @Last@-style instance
unionPred :: Eq s => PredSteps c s a -> PredSteps c s a -> PredSteps c s a
unionPred (PredSteps (xss@(PredStep i _ _ _):pxs)) (PredSteps (yss@(PredStep j _ _ _):pys))
  | i == j    = PredSteps $ yss : unPredSteps (unionPred (PredSteps pxs) (PredSteps pys))
  | otherwise = PredSteps $ xss : yss : unPredSteps (unionPred (PredSteps pxs) (PredSteps pys))
unionPred x (PredSteps []) = x
unionPred (PredSteps []) y = y
