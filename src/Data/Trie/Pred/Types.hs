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
import Data.Functor.Syntax
import qualified Data.Trie.Map as TM
import qualified Data.Map as M
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE


-- * Predicated Step

data PredStep c s a = forall r. PredStep
  { predPred :: s -> Maybe r
  , predData :: Maybe (r -> a)
  , predSub  :: c s (r -> a)
  }

instance Functor (c s) => Functor (PredStep c s) where
  fmap f (PredStep p mx xs) = PredStep p (f <.$> mx) $ f <.$> xs

-- | Lookup and delete only - can't arbitrarilly construct a predicated trie.
instance Trie NonEmpty s c => Trie NonEmpty s (PredStep c) where
  lookup (t:|ts) (PredStep p mx xs) = do
    r <- p t
    if null ts then mx <$~> r
               else lookup (NE.fromList ts) xs <$~> r
  delete (t:|ts) xss@(PredStep p mx xs) =
    maybe xss
      (const $ if null ts
               then PredStep p Nothing xs
               else PredStep p mx $ delete (NE.fromList ts) xs)
      (p t)



-- * Predicated Trie

data PredTrie s a = PredTrie
  { predLits  :: TM.MapStep PredTrie s a
  , predPreds :: [PredStep PredTrie s a]
  } deriving (Functor)
