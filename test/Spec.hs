module Main where

import Data.Trie.Pred.UnifiedSpec

import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing..."
  [unifiedSpec]
