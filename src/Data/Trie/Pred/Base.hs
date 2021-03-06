{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , DeriveFunctor
  , DeriveDataTypeable
  , TupleSections
  #-}

{- |
Module      : Data.Trie.Pred.Base
Copyright   : (c) 2015 Athan Clark

License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC

A "predicative" trie is a lookup table where you can use /predicates/
as a method to match a query path, where success is also enriched with /any/
auxiliary data. This library allows you to match a path-chunk (if you consider
a query to the different levels of the tree as a /list/) with a Boolean predicate,
augmented with existentially quantified data. This lets us use parsers, regular
expressions, and other functions that can be turned into the form of:

> forall a. p -> Maybe a

However, because the communicated data is existentially quantified, we __cannot__
revisit a definition - we cannot @update@ a predicative node, or change any of
its children. The current version of this library forces you to use 'PredTrie'
and 'RootedPredTrie' directly (i.e. the data constructors) to build your trie
manually.

This isn't the actual code, but it's a general idea for how you could build a
trie. We build a "tagged" <https://en.wikipedia.org/wiki/Rose_tree rose-tree>,
where each node has either a literal name (and is a singleton of the @k@ type in our
lookup path) or a predicate to consider the current node or its children as the target.
You could imagine a "step" of the trie structure as something like this:

> data PredTrie k a
>   = Nil
>   | Lit
>       { litTag       :: k
>       , litResult    :: Maybe a
>       , litChildren  :: Maybe (PredTrie k a)
>       }
>   | forall t. Pred
>       { predMatch    :: k -> Maybe t
>       , predResult   :: Maybe (t -> a)
>       , predChildren :: Maybe (PredTrie k a)
>       }

Notice how in the @Pred@ constructor, we first /create/ the @t@ data in @predMatch@,
then /consume/ it in @predResult@. We make a tree out of steps by recursing over the
steps.

This isn't how it's actually represented internally, but serves to help see the
representation. If you want to build tries
and perform lookups casually, please see the "Data.Trie.Pred.Interface" module.
-}

module Data.Trie.Pred.Base where

import Prelude hiding (lookup)
import Data.Trie.Pred.Base.Step (PredStep (..), Pred (..))
import Data.Trie.Class (Trie (..))
import qualified Data.Trie.HashMap as HT
import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Data (Typeable)
import Data.Monoid (First (..), Last (..), (<>))
import Data.Maybe (fromMaybe)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HMS
import Test.QuickCheck (Arbitrary (..))
import Control.DeepSeq (NFData (..))



-- * Predicated Trie

data PredTrie k a = PredTrie
  { predLits  :: !(HT.HashMapStep PredTrie k a) -- ^ a /literal/ step
  , predPreds :: PredStep k PredTrie k a      -- ^ a /predicative/ step
  } deriving (Show, Functor, Typeable)

instance (NFData k, NFData a) => NFData (PredTrie k a) where
  rnf (PredTrie hs ps) = rnf hs `seq` rnf ps

instance ( Arbitrary k
         , Arbitrary a
         , Eq k
         , Hashable k
         ) => Arbitrary (PredTrie k a) where
  arbitrary = flip PredTrie mempty <$> arbitrary

instance ( Hashable k
         , Eq k
         ) => Trie NonEmpty k PredTrie where
  lookup ts (PredTrie ls ps) =
    getFirst (First (lookup ts ls) <> First (lookup ts ps))
  delete ts (PredTrie ls ps) = PredTrie (delete ts ls) (delete ts ps)
  insert ts x (PredTrie ls ps) = PredTrie (HT.insert ts x ls) ps -- can only insert literals

instance ( Hashable k
         , Eq k
         ) => Semigroup (PredTrie k a) where
  (PredTrie ls1 ps1) <> (PredTrie ls2 ps2) =
    PredTrie (ls1 <> ls2) (ps1 <> ps2)

instance ( Hashable k
         , Eq k
         ) => Monoid (PredTrie k a) where
  mempty = PredTrie mempty mempty

emptyPT :: PredTrie k a
emptyPT = PredTrie HT.empty (PredStep HMS.empty)


-- subtrie :: Ord s => NonEmpty s -> PredTrie s a -> PredTrie s a
-- subtrie (t:|ts) (PredTrie (MapTrie (MapStep ls)) ps)
--   | null ts = getFirst $ First (lookup ts ls)

-- | Find the nearest parent node of the requested query, while returning
-- the split of the string that was matched, and what wasn't.
matchPT :: ( Hashable k
           , Eq k
           ) => NonEmpty k -> PredTrie k a -> Maybe (NonEmpty k, a, [k])
matchPT (t:|ts) (PredTrie ls (PredStep ps)) = getFirst $
  First (goLit ls) <> foldMap (First . goPred) ps
  where
    goLit (HT.HashMapStep xs) = do
      (HT.HashMapChildren mx mxs) <- HM.lookup t xs
      let mFoundHere = (t:|[],,[]) <$> mx
      if null ts
      then mFoundHere
      else getFirst $ First (do (pre,y,suff) <- matchPT (NE.fromList ts) =<< mxs
                                pure (NE.cons t pre, y, suff))
                   <> First mFoundHere

    goPred (Pred p mx xs) = do
      r <- p t
      let mFoundHere = do x <- ($ r) <$> mx
                          pure (t:|[], x, [])
      if null ts
      then mFoundHere
      else getFirst $ First (do (pre,y,suff) <- matchPT (NE.fromList ts) xs
                                pure (NE.cons t pre, y r, suff))
                   <> First mFoundHere


matchesPT :: ( Hashable k
             , Eq k
             ) => NonEmpty k -> PredTrie k a -> [(NonEmpty k, a, [k])]
matchesPT (t:|ts) (PredTrie ls (PredStep ps)) =
  fromMaybe [] (getFirst (First (goLit ls) <> foldMap (First . goPred) ps))
  where
    goLit (HT.HashMapStep xs) = do
      (HT.HashMapChildren mx mxs) <- HM.lookup t xs
      let mFoundHere = do x <- mx
                          pure [(t:|[],x,ts)]
          prependAncestry (pre,x,suff) = (NE.cons t pre,x,suff)
      if null ts
      then mFoundHere
      else do foundHere <- mFoundHere
              let rs = fromMaybe [] (matchesPT (NE.fromList ts) <$> mxs)
              pure (foundHere ++ (prependAncestry <$> rs))

    goPred (Pred p mx xs) = do
      r <- p t
      let mFoundHere = do x <- ($ r) <$> mx
                          pure [(t:|[],x,ts)]
          prependAncestryAndApply (pre,x,suff) = (NE.cons t pre,x r,suff)
      if null ts
      then mFoundHere
      else do foundHere <- mFoundHere
              let rs = matchesPT (NE.fromList ts) xs
              pure (foundHere ++ (prependAncestryAndApply <$> rs))

-- * Rooted Predicative Trie

data RootedPredTrie k a = RootedPredTrie
  { rootedBase :: !(Maybe a)      -- ^ The "root" node - the path at @[]@
  , rootedSub  :: !(PredTrie k a) -- ^ The actual predicative trie
  } deriving (Show, Functor, Typeable)

instance (NFData k, NFData a) => NFData (RootedPredTrie k a) where
  rnf (RootedPredTrie mx xs) = rnf mx `seq` rnf xs

instance ( Hashable k
         , Eq k
         ) => Trie [] k RootedPredTrie where
  lookup [] (RootedPredTrie mx _) = mx
  lookup ts (RootedPredTrie _ xs) = lookup (NE.fromList ts) xs

  delete [] (RootedPredTrie _ xs)  = RootedPredTrie Nothing xs
  delete ts (RootedPredTrie mx xs) = RootedPredTrie mx (delete (NE.fromList ts) xs)

  insert [] x (RootedPredTrie _ xs)  = RootedPredTrie (Just x) xs
  insert ts x (RootedPredTrie mx xs) = RootedPredTrie mx (insert (NE.fromList ts) x xs)


instance ( Hashable k
         , Eq k
         ) => Semigroup (RootedPredTrie k a) where
  (RootedPredTrie mx xs) <> (RootedPredTrie my ys) = RootedPredTrie
    (getLast (Last mx <> Last my)) (xs <> ys)


instance ( Hashable k
         , Eq k
         ) => Monoid (RootedPredTrie k a) where
  mempty = emptyRPT


emptyRPT :: RootedPredTrie k a
emptyRPT = RootedPredTrie Nothing emptyPT

matchRPT :: ( Hashable k
            , Eq k
            ) => [k] -> RootedPredTrie k a -> Maybe ([k], a, [k])
matchRPT [] (RootedPredTrie mx _)  = ([],,[]) <$> mx
matchRPT ts (RootedPredTrie mx xs) = getFirst $
  First mFoundThere <> First (([],,[]) <$> mx)
  where
    mFoundThere = do (pre,x,suff) <- matchPT (NE.fromList ts) xs
                     pure (NE.toList pre,x,suff)

matchesRPT :: ( Hashable k
              , Eq k
              ) => [k] -> RootedPredTrie k a -> [([k], a, [k])]
matchesRPT [] (RootedPredTrie mx _)  = fromMaybe [] $ (\x -> [([],x,[])]) <$> mx
matchesRPT ts (RootedPredTrie mx xs) =
  foundHere ++ fmap allowRoot  (matchesPT (NE.fromList ts) xs)
  where
    foundHere = fromMaybe [] $ do x <- mx
                                  pure [([],x,[])]
    allowRoot (pre,x,suff) = (NE.toList pre,x,suff)
