{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveFunctor
  , BangPatterns
  , FlexibleContexts
  , TypeFamilies
  #-}

{- |
Module      : Data.Trie.Pred.Interface
Copyright   : (c) 2015 Athan Clark

License     : BSD-style
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC

This module defines a "builder" monad, which aides in the process of building
a trie. It's a monad transformer, so you can use it alongside whichever
context you're already working in.

> myBuilder :: ( Eq k
>              , Hashable k
>              , MonadIO m
>              ) => PTBuilder String Int m ()
> myBuilder = do
>   insertHere 0
>   insert ("some" ./ "path" ./ nil) 1
>   insert ("some" ./ pred "pred-chunk" upperPred ./ nil) 2
>   prefix ("some") $ do
>     insert ("thing" ./ nil) 3
>     insert ("else" ./ nil) 4
>     data <- liftIO (doSomething)
>     insert ("another" ./ "thing" ./ nil) data
>   where
>     uppderPred :: String -> Maybe String
>     uppderPred s | all isUpperCase s = Just s
>                  | otherwise         = Nothing
>

Then we can get our trie to perform lookups by executing the monad:

> main :: IO ()
> main = do
>   trie <- execPTBuilderT myBuilder
>   print (lookup ["foo", "bar", "baz"] trie)

-}

module Data.Trie.Pred.Interface
  ( -- * Construction
    -- ** Builder Monad
    PTBuilderT (..)
  , execPTBuilderT
  , -- ** Combinators
    insert
  , insertHere
  , prefix
  , -- ** Specifying Paths
    only
  , pred
  , (./)
  , nil
  , -- * Query
    lookup
  , match
  , matches
  , -- * Delete
    delete
  , -- * Types
    RootedPredTrie
  , PathChunks
  , PathChunk
  ) where

import Prelude hiding (lookup, pred)
import Data.Trie.Pred.Base
import Data.Trie.Pred.Interface.Types
import Data.Function.Poly
import qualified Data.Trie.Class as TC

import Data.Hashable
import Data.Monoid
import Control.Monad.State
import Control.Monad.Writer


-- * Building Tries

newtype PTBuilderT k v m a = PTBuilderT
  { runPTBuilderT :: StateT (RootedPredTrie k v) m a
  } deriving ( Functor, Applicative, Monad, MonadTrans
             , MonadState (RootedPredTrie k v))

instance ( Monad m
         , Eq k
         , Hashable k
         ) => MonadWriter (RootedPredTrie k v) (PTBuilderT k v m) where
  tell x = modify' (x <>)
  listen x = do
    x' <- x
    w <- get
    return (x', w)
  pass x = do
    (x', f) <- x
    modify' f
    return x'


execPTBuilderT :: ( Monad m
                  , Eq k
                  , Hashable k
                  ) => PTBuilderT k v m a -> m (RootedPredTrie k v)
execPTBuilderT = flip execStateT mempty . runPTBuilderT


-- * Combinators

insert :: ( Monad m
          , Eq k
          , Hashable k
          , Singleton (PathChunks k xs)
              childContent
              (RootedPredTrie k resultContent)
          , cleanxs ~ CatMaybes xs
          , ArityTypeListIso childContent cleanxs resultContent
          ) => PathChunks k xs
            -> childContent
            -> PTBuilderT k resultContent m ()
insert !ts !vl =
  modify' (singleton ts vl <>)


insertHere :: ( Monad m
              , Eq k
              , Hashable k
              ) => v
                -> PTBuilderT k v m ()
insertHere = insert nil


prefix :: ( Monad m
          , Eq k
          , Hashable k
          , cleanxs ~ CatMaybes xs
          , ExtrudeSoundly k cleanxs xs childContent resultContent
          ) => PathChunks k xs
            -> PTBuilderT k childContent  m ()
            -> PTBuilderT k resultContent m ()
prefix !ts cs = do
  trie <- lift (execPTBuilderT cs)
  modify' (extrude ts trie <>)


lookup :: ( Eq k
          , Hashable k
          ) => [k] -> RootedPredTrie k a -> Maybe a
lookup = TC.lookup

delete :: ( Eq k
          , Hashable k
          ) => [k] -> RootedPredTrie k a -> RootedPredTrie k a
delete = TC.delete

match :: ( Hashable k
         , Eq k
         ) => [k] -> RootedPredTrie k a -> Maybe ([k], a, [k])
match = matchRPT


matches :: ( Hashable k
           , Eq k
           ) => [k] -> RootedPredTrie k a -> [([k], a, [k])]
matches = matchesRPT
