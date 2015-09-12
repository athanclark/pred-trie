module Data.Trie.Pred.UnifiedSpec (unifiedSpec) where

import Data.Trie.Pred.Unified as U
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck


unifiedSpec :: TestTree
unifiedSpec = testGroup "Data.Trie.Pred.Unified"
  [ testGroup "`lookup`"
      [ QC.testProperty "should work for present elements" prop_lookup_elem
      ]
  ]

prop_lookup_elem :: ExistingPath Int Int -> Bool
prop_lookup_elem (ExistingPath ts xs) = isJust $ U.lookup ts xs


data ExistingPath t x = ExistingPath
  { pathExisting :: [t]
  , trieExisting :: RUPTrie t x }
  deriving (Show, Eq)

instance (Arbitrary t, Arbitrary x, Eq t) => Arbitrary (ExistingPath t x) where
  arbitrary = do
    xs <- arbitrary
    ts <- arbitrary `suchThat` (`U.elem` xs)
    return $ ExistingPath ts xs
