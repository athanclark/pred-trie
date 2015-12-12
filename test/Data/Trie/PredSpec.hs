module Data.Trie.PredSpec where

import Data.Trie.Pred
import Data.Trie.Class
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Prelude hiding (lookup)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC


instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

predSpec :: TestTree
predSpec = testGroup "Data.Trie.Pred"
  [ testGroup "literals"
    [ QC.testProperty "lookup after insert should exist" lookupInsertExists
    , QC.testProperty "lookup after delete should not exist" lookupDeleteNotExists
    ]
  ]


lookupInsertExists :: NonEmpty Int -> Int -> PredTrie Int Int -> Bool
lookupInsertExists ks v xs = Just v == lookup ks (insert ks v xs)

lookupDeleteNotExists :: NonEmpty Int -> PredTrie Int Int -> Bool
lookupDeleteNotExists ks xs = Nothing == lookup ks (delete ks xs)
