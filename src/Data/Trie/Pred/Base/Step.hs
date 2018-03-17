{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , DeriveFunctor
  , DeriveDataTypeable
  , OverloadedLists
  , UndecidableInstances
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
import Data.Trie.Class (Trie (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Data (Typeable)
import Data.Monoid (First (..), (<>))
import Data.Maybe (maybe)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Control.DeepSeq (NFData (..))


-- * Single Predicated Step

data Pred c s a = forall r. Typeable r => Pred
  { -- | The predicate, existentially quantified in the successful result @r@
    predPred :: !(s -> Maybe r)
  , -- | The result function, capturing the quantified result @r@ and turning
    --   it into a top-level variable @a@.
    predData :: !(Maybe (r -> a))
  , -- | Any sub-trie must have __all__ results preceeded in arity with
    --   the result at this step.
    predSub  :: c s (r -> a)
  } deriving (Typeable)

instance Show (Pred c s a) where
  show (Pred _ mx _) = "Pred { predSub = ##, predData = " ++ maybe "Nothing" (\_ -> "Just ##") mx ++ " }"

instance Functor (c s) => Functor (Pred c s) where
  fmap f (Pred p mx xs) = Pred p ((f .) <$> mx) ((f .) <$> xs)

instance (Functor (c s), NFData (c s (s -> Maybe a))) => NFData (Pred c s a) where
  rnf (Pred p mx xs) =
    ( case mx of
        Nothing -> ()
        Just f -> rnf (\x -> case p x of
                               Nothing -> Nothing
                               Just r -> Just (f r)
                          )
      ) `seq` rnf ((\f -> (\x -> case p x of
                              Nothing -> Nothing
                              Just r -> Just (f r)
                             )
                   ) <$> xs)


-- | Lookup and delete only - can't arbitrarilly construct a predicated trie.
instance Trie NonEmpty s c => Trie NonEmpty s (Pred c) where
  lookup (t:|ts) (Pred p mx xs) = do
    r <- p t
    fmap ($ r) $
      if null ts
      then mx
      else lookup (NE.fromList ts) xs
  delete (t:|ts) xss@(Pred p mx xs) =
    case p t of
      Nothing -> xss
      Just _
        | null ts -> Pred p Nothing xs
        | otherwise -> Pred p mx (delete (NE.fromList ts) xs)


singletonPred :: ( Monoid (c s (r -> a))
                 , Typeable r
                 ) => (s -> Maybe r) -> (r -> a) -> Pred c s a
singletonPred p x = Pred p (Just x) mempty


-- * Adjacent Predicated Steps

-- | Adjacent steps
newtype PredStep k c s a = PredStep
  { unPredSteps :: HashMap k (Pred c s a)
  } deriving (Show, Functor, Typeable)


instance (Functor (c s), NFData (c s (s -> Maybe a)), NFData k) => NFData (PredStep k c s a) where
  rnf (PredStep xs) = rnf xs

-- | Lookup and delete only - can't arbitrarilly construct a predicated trie.
instance Trie NonEmpty s c => Trie NonEmpty s (PredStep k c) where
  lookup ts (PredStep ps) = getFirst (foldMap (First . lookup ts) ps)
  delete ts (PredStep ps) = PredStep (fmap (delete ts) ps)

instance ( Eq k
         , Hashable k
         ) => Monoid (PredStep k c s a) where
  mempty  = PredStep HMS.empty
  mappend = unionPred

-- | overwrite on the right
unionPred :: ( Eq k
             , Hashable k
             ) => PredStep k c s a
               -> PredStep k c s a
               -> PredStep k c s a
unionPred (PredStep xs) (PredStep ys) = PredStep (xs <> ys)
