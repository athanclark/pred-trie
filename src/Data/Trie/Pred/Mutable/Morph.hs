module Data.Trie.Pred.Mutable.Morph where

import Data.Trie.Pred.Mutable as M
import Data.Trie.Pred.Base as B
import Data.Trie.Pred.Base.Step as B

import Data.PredSet.Mutable as HS
import Data.Trie.HashMap as HMT
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.HashTable.ST.Basic (HashTable)
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.Map.Strict as Map

import Control.Monad.ST
import Data.Foldable (foldlM)
import Data.Typeable
import Data.Dynamic
import Data.Hashable
import Data.Proxy
import Data.STRef



toMutable :: ( Eq k
             , Hashable k
             , Ord k
             , Typeable s
             , Typeable k
             , Typeable a
             ) => PredTrie k a
               -> ST s (HashTableTrie s k a)
toMutable xs = do
  predRefs <- newSTRef Map.empty
  predSet  <- HS.new
  toHashTableTrie predRefs predSet xs


toHashTableTrie :: ( Eq k
                   , Hashable k
                   , Ord k
                   , Typeable s
                   , Typeable k
                   , Typeable a
                   ) => STRef s (HMap k)
                     -> PredSet s k
                     -> PredTrie k a
                     -> ST s (HashTableTrie s k a)
toHashTableTrie predRefs predSet (PredTrie (HashMapStep raw) (PredSteps preds)) = do
  raw' <- toHashTable =<< traverse (toRawValue predRefs predSet) raw
  preds' <- mapM (toMutablePredStep predRefs predSet) preds
  pure (HashTableTrie raw' preds')

toRawValue :: ( Eq k
              , Hashable k
              , Ord k
              , Typeable s
              , Typeable k
              , Typeable a
              ) => STRef s (HMap k)
                -> PredSet s k
                -> HashMapChildren PredTrie k a
                -> ST s (RawValue s k a)
toRawValue predRefs predSet (HashMapChildren mx mchildren) = do
  children <- case mchildren of
                Nothing -> M.new
                Just xs -> toHashTableTrie predRefs predSet xs
  pure (RawValue mx children)

toHashTable :: ( Eq k
               , Hashable k
               ) => HM.HashMap k a
                 -> ST s (HashTable s k a)
toHashTable xs = do
  fresh <- HT.new
  foldlM (\() (k,v) -> HT.insert fresh k v) () (HM.toList xs)
  pure fresh

toMutablePredStep :: ( Ord k
                     , Eq k
                     , Hashable k
                     , Typeable s
                     , Typeable k
                     , Typeable a
                     ) => STRef s (HMap k)
                       -> PredSet s k
                       -> B.PredStep k PredTrie k a
                       -> ST s (M.PredStep s k a)
toMutablePredStep predRefs predSet (B.PredStep tag pred mx children) = do
  predRefs' <- readSTRef predRefs
  mPredKey <- lookupPredKey tag (pure pred) predRefs'
  predKey  <- case mPredKey of
                Nothing -> do predKey' <- HS.insert pred predSet
                              writeSTRef predRefs (insertPredKey tag predKey' predRefs')
                              pure predKey'
                Just x  -> pure x
  children' <- toHashTableTrie predRefs predSet children
  pure (M.PredStep predKey mx children')


-- Wiiiked abuse of the type system

type HMap k = Map.Map k Dynamic

insertPredKey :: ( Ord k'
                 , Typeable s
                 , Typeable k
                 , Typeable a
                 ) => k'
                   -> PredKey s k a
                   -> HMap k'
                   -> HMap k'
insertPredKey k pred = Map.insert k (toDyn pred)

lookupPredKey :: ( Ord k'
                 , Typeable s
                 , Typeable k
                 , Typeable a
                 ) => k'
                   -> ST s (k -> Maybe a)
                   -> HMap k'
                   -> ST s (Maybe (PredKey s k a))
lookupPredKey k pred xs = do
  pred' <- pred
  pure $! fromDynamic =<< Map.lookup k xs
