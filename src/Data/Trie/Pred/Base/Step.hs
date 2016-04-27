{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , DeriveFunctor
  , DeriveDataTypeable
  #-}

{- |
Module      : Data.Trie.Pred.Base.Step
Copyright   : (c) 2015 Athan Clark

License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC
-}

module Data.Trie.Pred.Base.Step where

import Prelude hiding (lookup)
import Data.Trie.Class
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Typeable
import Data.Functor.Syntax
import Data.Monoid


-- * Single Predicated Step

data PredStep k c s a = forall r. Typeable r => PredStep
  { -- | Unique identifier for the predicate - used for combination
    predTag  :: !k
  , -- | The predicate, existentially quantified in the successful result @r@
    predPred :: !(s -> Maybe r)
  , -- | The result function, capturing the quantified result @r@ and turning
    --   it into a top-level variable @a@.
    predData :: !(Maybe (r -> a))
  , -- | Any sub-trie must have __all__ results preceeded in arity with
    --   the result at this step.
    predSub  :: !(c s (r -> a))
  } deriving (Typeable)

instance ( Show s
         , Show k
         ) => Show (PredStep k c s a) where
  show (PredStep t _ _ _) = "PredStep {predTag=" ++ show t ++ ", ...}"

instance Functor (c s) => Functor (PredStep k c s) where
  fmap f (PredStep i p mx xs) = (PredStep i p $! f <.$> mx) $! f <.$> xs

-- | Lookup and delete only - can't arbitrarilly construct a predicated trie.
instance Trie NonEmpty s c => Trie NonEmpty s (PredStep k c) where
  lookup (t:|ts) (PredStep _ p mx xs) = do
    r <- p t
    if null ts then mx <$~> r
               else lookup (NE.fromList ts) xs <$~> r
  delete (t:|ts) xss@(PredStep i p mx xs) =
    maybe xss
      (const $ if null ts
               then PredStep i p Nothing xs
               else PredStep i p mx $! delete (NE.fromList ts) xs)
      (p t)

singletonPred :: ( Monoid (c s (r -> a))
                 , Typeable r
                 ) => k -> (s -> Maybe r) -> (r -> a) -> PredStep k c s a
singletonPred i p x = PredStep i p (Just x) mempty


-- * Adjacent Predicated Steps

-- | Adjacent steps
newtype PredSteps k c s a = PredSteps
  { unPredSteps :: [PredStep k c s a]
  } deriving (Show, Functor, Typeable)

-- | Lookup and delete only - can't arbitrarilly construct a predicated trie.
instance Trie NonEmpty s c => Trie NonEmpty s (PredSteps k c) where
  lookup ts (PredSteps ps) = getFirst $! foldMap (First . lookup ts) ps
  delete ts (PredSteps ps) = PredSteps $! fmap (delete ts) ps

instance ( Eq s
         , Eq k
         ) => Monoid (PredSteps k c s a) where
  mempty  = PredSteps []
  mappend = unionPred

-- | @Last@-style instance
unionPred :: ( Eq k
             ) => PredSteps k c s a
               -> PredSteps k c s a
               -> PredSteps k c s a
unionPred (PredSteps (xss@(PredStep i _ _ _):pxs)) (PredSteps (yss@(PredStep j _ _ _):pys))
  | i == j    = PredSteps $ yss :       (unPredSteps $! unionPred (PredSteps pxs) (PredSteps pys))
  | otherwise = PredSteps $ xss : yss : (unPredSteps $! unionPred (PredSteps pxs) (PredSteps pys))
unionPred x (PredSteps []) = x
unionPred (PredSteps []) y = y
