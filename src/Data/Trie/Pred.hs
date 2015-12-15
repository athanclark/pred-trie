{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , DeriveFunctor
  , DeriveGeneric
  , DeriveDataTypeable
  , TupleSections
  , BangPatterns
  #-}

{- |
Module      : Data.Trie.Pred
Copyright   : (c) 2015 Athan Clark

License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC

A "predicative" trie is a lookup table where you can embed arbitrary predicates
as a method to satisfy a node as "found" - this is done with existential quantification.
To embed our predicates, we need to build the trie's data constructors manually,
to unify the existential data with the the result function.

As a botched example, you could imagine a "step" of the trie structure as something
like this:

> PredTrie s a
>   = PNil
>   | forall t. PCons
>       { predicate :: s -> Maybe t
>       , result    :: t -> a
>       }

This isn't how it's actually represented, of course - this doesn't acocunt for
/literal/ matches (i.e. enumerated results).
-}

module Data.Trie.Pred where

import Prelude hiding (lookup)
import Data.Trie.Pred.Step
import Data.Trie.Class
import qualified Data.Trie.HashMap as HT
import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Typeable
import Data.Functor.Syntax
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Hashable
import Test.QuickCheck



-- * Predicated Trie

data PredTrie s a = PredTrie
  { predLits  :: !(HT.HashMapStep PredTrie s a) -- ^ a /literal/ step
  , predPreds :: !(PredSteps PredTrie s a)      -- ^ a /predicative/ step
  } deriving (Show, Functor, Typeable)

instance ( Arbitrary s
         , Arbitrary a
         , Eq s
         , Hashable s
         ) => Arbitrary (PredTrie s a) where
  arbitrary = (flip PredTrie $ PredSteps []) <$> arbitrary

instance ( Hashable s
         , Eq s
         ) => Trie NonEmpty s PredTrie where
  lookup ts (PredTrie ls ps) =
    getFirst $ (First $! lookup ts ls) <> First (lookup ts ps)
  delete ts (PredTrie ls ps) = PredTrie (delete ts ls) (delete ts ps)
  insert ts x (PredTrie ls ps) = PredTrie (HT.insert ts x ls) ps -- can only insert literals

instance ( Hashable s
         , Eq s
         ) => Monoid (PredTrie s a) where
  mempty = PredTrie mempty mempty
  mappend (PredTrie ls1 ps1) (PredTrie ls2 ps2) =
    (PredTrie $! ls1 <> ls2) $! ps1 <> ps2

emptyPT :: PredTrie s a
emptyPT = PredTrie HT.empty (PredSteps [])



-- subtrie :: Ord s => NonEmpty s -> PredTrie s a -> PredTrie s a
-- subtrie (t:|ts) (PredTrie (MapTrie (MapStep ls)) ps)
--   | null ts = getFirst $ First (lookup ts ls)

-- | Find the nearest parent node of the requested query, while returning
-- the split of the string that was matched, and what wasn't.
matchPT :: ( Hashable s
           , Eq s
           ) => NonEmpty s -> PredTrie s a -> Maybe (NonEmpty s, a, [s])
matchPT (t:|ts) (PredTrie ls (PredSteps ps)) = getFirst $
  First (goLit ls) <> foldMap (First . goPred) ps
  where
    goLit (HT.HashMapStep xs) = do
      (HT.HashMapChildren mx mxs) <- HM.lookup t xs
      let mFoundHere = (t:|[],, []) <$> mx
      if null ts
      then mFoundHere
      else getFirst $ First (do (pre,y,suff) <- matchPT (NE.fromList ts) =<< mxs
                                return (t:|NE.toList pre, y, suff))
                   <> First mFoundHere

    goPred (PredStep _ p mx xs) = do
      r <- p t
      let mFoundHere = do x <- mx <$~> r
                          return (t:|[], x, [])
      if null ts
      then mFoundHere
      else getFirst $ First (do (pre,y,suff) <- matchPT (NE.fromList ts) xs
                                return (t:|NE.toList pre, y r, suff))
                   <> First mFoundHere


matchesPT :: ( Hashable s
             , Eq s
             ) => NonEmpty s -> PredTrie s a -> [(NonEmpty s, a, [s])]
matchesPT (t:|ts) (PredTrie ls (PredSteps ps)) =
  fromMaybe [] $ getFirst $ First (goLit ls) <> foldMap (First . goPred) ps
  where
    goLit (HT.HashMapStep xs) = do
      (HT.HashMapChildren mx mxs) <- HM.lookup t xs
      let mFoundHere = do x <- mx
                          return [(t:|[],x,ts)]
          prependAncestry (pre,x,suff) = (t:| NE.toList pre,x,suff)
      if null ts
      then mFoundHere
      else do foundHere <- mFoundHere
              let rs = fromMaybe [] $! matchesPT (NE.fromList ts) <$> mxs
              return $! foundHere ++ (prependAncestry <$> rs)

    goPred (PredStep _ p mx xs) = do
      r <- p t
      let mFoundHere = do x <- mx <$~> r
                          return [(t:|[],x,ts)]
          prependAncestryAndApply (pre,x,suff) = (t:| NE.toList pre,x r,suff)
      if null ts
      then mFoundHere
      else do foundHere <- mFoundHere
              let rs = matchesPT (NE.fromList ts) xs
              return $! foundHere ++ (prependAncestryAndApply <$> rs)

-- * Rooted Predicated Trie

data RootedPredTrie s a = RootedPredTrie
  { rootedBase :: !(Maybe a)      -- ^ The "root" node - the path at @[]@
  , rootedSub  :: !(PredTrie s a) -- ^ The actual predicative trie
  } deriving (Show, Functor, Typeable)


instance ( Hashable s
         , Eq s
         ) => Trie [] s RootedPredTrie where
  lookup [] (RootedPredTrie mx _) = mx
  lookup ts (RootedPredTrie _ xs) = lookup (NE.fromList ts) xs

  delete [] (RootedPredTrie _ xs)  = RootedPredTrie Nothing xs
  delete ts (RootedPredTrie mx xs) = RootedPredTrie mx $! delete (NE.fromList ts) xs

  insert [] x (RootedPredTrie _ xs)  = RootedPredTrie (Just x) xs
  insert ts x (RootedPredTrie mx xs) = RootedPredTrie mx $! insert (NE.fromList ts) x xs


instance ( Hashable s
         , Eq s
         ) => Monoid (RootedPredTrie s a) where
  mempty = emptyRPT
  mappend (RootedPredTrie mx xs) (RootedPredTrie my ys) = RootedPredTrie
    (getLast $! Last mx <> Last my) $! xs <> ys


emptyRPT :: RootedPredTrie s a
emptyRPT = RootedPredTrie Nothing emptyPT

matchRPT :: ( Hashable s
            , Eq s
            ) => [s] -> RootedPredTrie s a -> Maybe ([s], a, [s])
matchRPT [] (RootedPredTrie mx _)  = ([],,[]) <$> mx
matchRPT ts (RootedPredTrie mx xs) = getFirst $
  First mFoundThere <> First (([],,[]) <$> mx)
  where
    mFoundThere = do (pre,x,suff) <- matchPT (NE.fromList ts) xs
                     pure (NE.toList pre,x,suff)

matchesRPT :: ( Hashable s
              , Eq s
              ) => [s] -> RootedPredTrie s a -> [([s], a, [s])]
matchesRPT [] (RootedPredTrie mx _)  = fromMaybe [] $ (\x -> [([],x,[])]) <$> mx
matchesRPT ts (RootedPredTrie mx xs) =
  foundHere ++ fmap allowRoot (matchesPT (NE.fromList ts) xs)
  where
    foundHere = fromMaybe [] $! (\x -> [([],x,[])]) <$> mx
    allowRoot (pre,x,suff) = (NE.toList pre,x,suff)
