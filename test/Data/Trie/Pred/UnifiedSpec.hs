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
  -- , testGroup "LinVar"
  --     [ QC.testProperty "should generate non-empty variable names"
  --         prop_linVar_notNull
  --     ]
  -- , testGroup "Ineq"
  --     [ QC.testProperty "`standardize` should be idempotent"
  --         prop_standardize_Idempotency
  --     ]
  ]

prop_lookup_elem :: ExistingPath Int Int -> Bool
prop_lookup_elem (ExistingPath xs ts) = isJust $ U.lookup ts xs


data ExistingPath t x = ExistingPath
  { trieExisting :: RUPTrie t x
  , pathExisting :: [t] }
  deriving (Show, Eq)

instance (Arbitrary t, Arbitrary x, Eq t) => Arbitrary (ExistingPath t x) where
  arbitrary = do
    xs <- arbitrary
    ts <- arbitrary `suchThat` (\x -> U.elem x xs)
    return $ ExistingPath xs ts
