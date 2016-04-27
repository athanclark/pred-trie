{-# LANGUAGE
    ExistentialQuantification
  #-}

module Data.Trie.Pred.Mutable where

import Prelude hiding (lookup)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM)
import Data.Typeable

import Data.List.NonEmpty hiding (insert)

import           Data.HashTable.ST.Basic (HashTable)
import qualified Data.HashTable.ST.Basic as HT
import           Data.PredSet.Mutable (PredSet, PredKey)
import qualified Data.PredSet.Mutable as HS
import Control.Monad.ST
import Data.Hashable


data PredStep s k r = forall a. Typeable a => PredStep
  { predPred :: {-# UNPACK #-} !(PredKey s k a)
  , predData :: !(Maybe (a -> r))
  , predSub  :: !(HashTableTrie s k (a -> r))
  }

data RawValue s k a = RawValue
  { rawValue    :: !(Maybe a)
  , rawChildren :: !(HashTableTrie s k a)
  }

data HashTableTrie s k a = HashTableTrie
  { rawValues :: {-# UNPACK #-} !(HashTable s k (RawValue s k a))
  , predPreds :: [PredStep s k a]
  }


new :: ST s (HashTableTrie s k a)
new = flip HashTableTrie [] <$> HT.new

insert :: ( Eq k
          , Hashable k
          ) => NonEmpty k
            -> a
            -> HashTableTrie s k a
            -> ST s (HashTableTrie s k a)
insert (k:|ks) x ref@(HashTableTrie raw _) =
  case ks of
    [] -> do
      mx' <- HT.lookup raw k
      case mx' of
        Nothing -> do
          children <- new
          HT.insert raw k $! RawValue (Just x) children
          pure ref
        Just (RawValue _ children) -> do
          HT.insert raw k $! RawValue (Just x) children
          pure ref
    (k':ks') -> do
      mx' <- HT.lookup raw k
      case mx' of
        Nothing -> do
          children <- new
          children' <- insert (k':|ks') x children
          HT.insert raw k $! RawValue Nothing children'
          pure ref
        Just (RawValue mx children) -> do
          children' <- insert (k':|ks') x children
          HT.insert raw k $! RawValue mx children'
          pure ref


lookup :: ( Eq k
          , Hashable k
          , Typeable s
          , Typeable k
          ) => PredSet s k
            -> NonEmpty k
            -> HashTableTrie s k a
            -> ST s (Maybe a)
lookup predSet (k:|ks) (HashTableTrie raw preds) = do
  mx <- HT.lookup raw k
  case mx of
    Just (RawValue mx' children) ->
      case ks of
        []       -> pure mx'
        (k':ks') -> lookup predSet (k':|ks') children
    Nothing ->
      let -- go :: Typeable t => Maybe t -> PredStep s k t -> ST s (Maybe t)
          go solution@(Just _) _                          = pure solution
          go Nothing (PredStep predKey mHandler children) = do
            mx' <- HS.lookup predKey k predSet
            case mx' of
              Nothing -> pure Nothing
              Just x  ->
                case ks of
                  [] ->
                    pure $! ($ x) <$> mHandler
                  (k':ks') -> do
                    mf <- lookup predSet (k':|ks') children
                    pure $! ($ x) <$> mf
      in  foldlM go Nothing preds


match :: ( Eq k
         , Hashable k
         , Typeable s
         , Typeable k
         ) => PredSet s k
           -> NonEmpty k
           -> HashTableTrie s k a
           -> ST s (Maybe (NonEmpty k, a, [k]))
match predSet (k:|ks) (HashTableTrie raw preds) = do
  mLit <- goLit raw
  case mLit of
    Just _  -> pure mLit
    Nothing ->
      let go solution@(Just _) _ = pure solution
          go Nothing pred        = goPred pred
      in  foldlM go Nothing preds
  where
    goLit xs = do
      mx' <- HT.lookup raw k
      case mx' of
        Nothing -> pure Nothing
        Just (RawValue mx children) ->
          let mFoundHere = (\x -> (k:|[], x, ks)) <$> mx
              prependAncestry (pre,x,suff) = (k:|toList pre,x,suff)
          in case ks of
            [] -> pure mFoundHere
            (k':ks') -> do
              mFoundThere <- match predSet (k':|ks') children
              pure $! getFirst $
                   First (prependAncestry <$> mFoundThere)
                <> First mFoundHere

    goPred (PredStep predKey mx children) = do
      mr' <- HS.lookup predKey k predSet
      case mr' of
        Nothing -> pure Nothing
        Just r  ->
          let mFoundHere = (\x -> (k:|[], x r, ks)) <$> mx
              prependAncestryAndApply (pre,f,suff) =
                (k:|toList pre,f r,suff)
          in case ks of
            [] -> pure mFoundHere
            (k':ks') -> do
              mFoundThere <- match predSet (k':|ks') children
              pure $! getFirst $
                   First (prependAncestryAndApply <$> mFoundThere)
                <> First mFoundHere

matches :: ( Eq k
           , Hashable k
           , Typeable s
           , Typeable k
           ) => PredSet s k
             -> NonEmpty k
             -> HashTableTrie s k a
             -> ST s [(NonEmpty k, a, [k])]
matches predSet (k:|ks) (HashTableTrie raw preds) = do
  mLit <- goLit raw
  case mLit of
    Just lit -> pure lit
    Nothing ->
      let go solution@(Just _) _ = pure solution
          go Nothing pred        = goPred pred
      in  fromMaybe [] <$> foldlM go Nothing preds
  where
    goLit xs = do
      mx' <- HT.lookup raw k
      case mx' of
        Nothing -> pure Nothing
        Just (RawValue mx children) ->
          let mFoundHere = (\x -> [(k:|[], x, ks)]) <$> mx
              prependAncestry (pre,x,suff) = (k:|toList pre, x, suff)
          in case ks of
            [] -> pure mFoundHere
            (k':ks') ->
              case mFoundHere of
                Nothing -> pure Nothing
                Just foundHere -> do
                  foundThere <- matches predSet (k':|ks') children
                  pure . Just $! foundHere ++ (prependAncestry <$> foundThere)


    goPred (PredStep predKey mx children) = do
      mr <- HS.lookup predKey k predSet
      case mr of
        Nothing -> pure Nothing
        Just r  ->
          let mFoundHere = (\f -> [(k:|[],f r,ks)]) <$> mx
              prependAncestryAndApply (pre,f,suff) =
                (k:|toList pre,f r,suff)
          in case ks of
            [] -> pure mFoundHere
            (k':ks') ->
              case mFoundHere of
                Nothing -> pure Nothing
                Just foundHere -> do
                  foundThere <- matches predSet (k':|ks') children
                  pure . Just $! foundHere ++ (prependAncestryAndApply <$> foundThere)
