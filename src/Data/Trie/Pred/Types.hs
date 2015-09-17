{-# LANGUAGE
    ExistentialQuantification
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , DeriveFunctor
  #-}

module Data.Trie.Pred.Types where

import Prelude hiding (lookup)
import Data.Trie.Pred.Step
import Data.Trie.Class
import qualified Data.Trie.Map as MT
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Functor.Syntax
import Data.Monoid
import Data.Maybe (fromMaybe)



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

emptyPT :: PredTrie s a
emptyPT = PredTrie MT.empty (PredSteps [])

-- subtrie :: Ord s => NonEmpty s -> PredTrie s a -> PredTrie s a
-- subtrie (t:|ts) (PredTrie (MapTrie (MapStep ls)) ps)
--   | null ts = getFirst $ First (lookup ts ls)

-- | Find the nearest parent node of the requested query, while returning
-- the split of the string that was matched, and what wasn't.
matchPT :: Ord s => NonEmpty s -> PredTrie s a -> Maybe (NonEmpty s, a, [s])
matchPT (t:|ts) (PredTrie ls (PredSteps ps)) = getFirst $
  First (goLit ls) <> foldMap (First . goPred) ps
  where
    goLit (MT.MapStep xs) = do
      (mx,mxs) <- Map.lookup t xs
      let mFoundHere = do x <- mx
                          return (t:|[], x, [])
      if null ts then mFoundHere
      else getFirst $ First (do (pre,y,suff) <- matchPT (NE.fromList ts) =<< mxs
                                return (t:|NE.toList pre, y, suff))
                   <> First mFoundHere

    goPred (PredStep _ p mx xs) = do
      r <- p t
      let mFoundHere = do x <- mx <$~> r
                          return (t:|[], x, [])
      if null ts then mFoundHere
      else getFirst $ First (do (pre,y,suff) <- matchPT (NE.fromList ts) xs
                                return (t:|NE.toList pre, y r, suff))
                   <> First mFoundHere


matchesPT :: Ord s => NonEmpty s -> PredTrie s a -> [(NonEmpty s, a, [s])]
matchesPT (t:|ts) (PredTrie ls (PredSteps ps)) =
  fromMaybe [] $ getFirst $ First (goLit ls) <> foldMap (First . goPred) ps
  where
    goLit (MT.MapStep xs) = do
      (mx,mxs) <- Map.lookup t xs
      let mFoundHere = do x <- mx
                          return [(t:|[],x,ts)]
          prependAncestry (pre,x,suff) = (t:| NE.toList pre,x,suff)
      if null ts then mFoundHere
      else do foundHere <- mFoundHere
              let rs = fromMaybe [] $ matchesPT (NE.fromList ts) <$> mxs
              return $ foundHere ++ (prependAncestry <$> rs)

    goPred (PredStep _ p mx xs) = do
      r <- p t
      let mFoundHere = do x <- mx <$~> r
                          return [(t:|[],x,ts)]
          prependAncestryAndApply (pre,x,suff) = (t:| NE.toList pre,x r,suff)
      if null ts then mFoundHere
      else do foundHere <- mFoundHere
              let rs = matchesPT (NE.fromList ts) xs
              return $ foundHere ++ (prependAncestryAndApply <$> rs)

-- * Rooted Predicated Trie

data RootedPredTrie s a = RootedPredTrie
  { rootedBase :: Maybe a
  , rootedSub  :: PredTrie s a
  } deriving (Functor)

instance Ord s => Trie [] s RootedPredTrie where
  lookup [] (RootedPredTrie mx _) = mx
  lookup ts (RootedPredTrie _ xs) = lookup (NE.fromList ts) xs

  delete [] (RootedPredTrie _ xs) = RootedPredTrie Nothing xs
  delete ts (RootedPredTrie mx xs) = RootedPredTrie mx $ delete (NE.fromList ts) xs

  insert [] x (RootedPredTrie _ xs) = RootedPredTrie (Just x) xs
  insert ts x (RootedPredTrie mx xs) = RootedPredTrie mx $ insert (NE.fromList ts) x xs

instance Ord s => Monoid (RootedPredTrie s a) where
  mempty = RootedPredTrie Nothing emptyPT
  mappend (RootedPredTrie mx xs) (RootedPredTrie my ys) = RootedPredTrie
    (getLast $ Last mx <> Last my) $ xs <> ys


matchRPT :: Ord s => [s] -> RootedPredTrie s a -> Maybe ([s], a, [s])
matchRPT [] (RootedPredTrie mx _) = do x <- mx
                                       return ([],x,[])
matchRPT ts (RootedPredTrie mx xs) = getFirst $
  First mFoundThere <> First (do x <- mx
                                 return ([],x,[]))
  where mFoundThere = do (pre,x,suff) <- matchPT (NE.fromList ts) xs
                         return (NE.toList pre,x,suff)
