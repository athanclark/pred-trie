{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.Trie.PredSpec where

import Data.Trie.Pred.Base (PredTrie (..), emptyPT)
import Data.Trie.Pred.Base.Step (PredStep (..), Pred (..))
import Data.Trie.Class (Trie (lookup, insert, delete))
import Data.Trie.HashMap (HashMapStep (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import qualified Data.HashMap.Strict as HM
import Data.Attoparsec.Text (parseOnly, double)
import qualified Data.Text as T
import Control.Error (hush)
import Prelude hiding (lookup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC



predSpec :: TestTree
predSpec = testGroup "Data.Trie.Pred"
  [ testGroup "literals"
    [ QC.testProperty "lookup after insert should exist" lookupInsertExists
    , QC.testProperty "lookup after delete should not exist" lookupDeleteNotExists
    ]
  , testGroup "predicates"
    [ QC.testProperty "any double is parsed by a double" lookupDouble
    ]
  ]


lookupInsertExists :: NonEmpty Int -> Int -> PredTrie Int Int -> Bool
lookupInsertExists ks v xs = Just v == lookup ks (insert ks v xs)

lookupDeleteNotExists :: NonEmpty Int -> PredTrie Int Int -> Bool
lookupDeleteNotExists ks xs = Nothing == lookup ks (delete ks xs)

lookupDouble :: Double -> Bool
lookupDouble d = Just 0 == lookup ((T.pack $ show d) :| []) doubleTable


doubleTable :: PredTrie T.Text Int
doubleTable = PredTrie (HashMapStep HM.empty) $
  PredStep (HM.singleton "double" (Pred (hush . parseOnly double) (Just $ \d -> 0) emptyPT))
