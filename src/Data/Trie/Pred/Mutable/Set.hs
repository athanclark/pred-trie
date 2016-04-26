module Data.Trie.Pred.Mutable.Set where

import Data.Monoid

import           Data.HSet.Mutable (HSet, HKey)
import qualified Data.HSet.Mutable as HS
import           Data.HashTable.ST.Cuckoo (HashTable)
import qualified Data.HashTable.ST.Cuckoo as HT
import Data.Typeable
import Data.Hashable
import Control.Monad.ST



data CachedPred s k a = CachedPred
  { cachedPredPred  :: !(k -> Maybe a)
  , cachedPredCache :: {-# UNPACK #-} !(HashTable s k a)
  }

newCachedPred :: (k -> Maybe a) -> ST s (CachedPred s k a)
newCachedPred pred = CachedPred pred <$> HT.new

query :: ( Eq k
         , Hashable k
         ) => k -> CachedPred s k a -> ST s (Maybe a)
query k (CachedPred pred cache) =
      (\mx -> getFirst $! First mx <> First (pred k))
  <$> HT.lookup cache k


newtype PredSet k s = PredSet
  { getPredSet :: HSet s
  }

newtype PredKey k a = PredKey
  { getPredKey :: HKey (k -> Maybe a)
  }


new :: ST s (PredSet k s)
new = PredSet <$> HS.new

insert :: ( Typeable k
          , Typeable a
          ) => (k -> Maybe a)
            -> PredSet k s
            -> ST s (PredKey k a)
insert pred (PredSet xs) = PredKey <$> HS.insert pred xs
