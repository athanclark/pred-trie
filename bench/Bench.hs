{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where


import           Data.Trie.Pred.Unified
import qualified Data.Trie.Pred.Unified as U
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Criterion.Main


doubleLit :: RUPTrie T.Text Double
doubleLit = Rooted Nothing
  [ UMore "1" (Just 1) []
  , UMore "2" (Just 2) []
  , UMore "3" (Just 3) []
  , UMore "4" (Just 4) []
  ]

doubleAtto :: RUPTrie T.Text Double
doubleAtto = Rooted Nothing
  [ UPred "d" (eitherToMaybe . parseOnly double) (Just id) []
  ]
  where
    eitherToMaybe (Left _) = Nothing
    eitherToMaybe (Right a) = Just a


main = defaultMain
  [ bgroup "Lit vs. Pred"
    [ bgroup "Lit"
      [ bench "1" $ whnf (U.lookup ["1"]) doubleLit
      , bench "2" $ whnf (U.lookup ["2"]) doubleLit
      , bench "3" $ whnf (U.lookup ["3"]) doubleLit
      , bench "4" $ whnf (U.lookup ["4"]) doubleLit
      ]
    , bgroup "Pred"
      [ bench "1" $ whnf (U.lookup ["1"]) doubleAtto
      , bench "2" $ whnf (U.lookup ["2"]) doubleAtto
      , bench "3" $ whnf (U.lookup ["3"]) doubleAtto
      , bench "4" $ whnf (U.lookup ["4"]) doubleAtto
      ]
    ]
  ]
