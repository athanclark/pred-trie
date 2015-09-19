{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where


import Prelude hiding (lookup)
import           Data.Trie.Pred
import           Data.Trie.Pred.Step (PredStep (..), PredSteps (..))
import           Data.Trie.Class
import           Data.Trie.Map (MapStep (..))
import qualified Data.Map as Map
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Criterion.Main
import           Data.Set.Class as Sets


doubleLit :: RootedPredTrie T.Text Double
doubleLit = RootedPredTrie Nothing $ PredTrie
              (MapStep $ unUnion $ foldMap (Union . genStep) [1..100])
              (PredSteps [])
  where genStep n = Map.singleton (T.pack $ show n) (Just n, Nothing)

doubleAtto :: RootedPredTrie T.Text Double
doubleAtto = RootedPredTrie Nothing $ PredTrie mempty $ PredSteps
  [PredStep "d" (eitherToMaybe . parseOnly double) (Just id) mempty]
  where
    eitherToMaybe (Left _) = Nothing
    eitherToMaybe (Right a) = Just a


main = defaultMain
  [ bgroup "Lit vs. Pred"
    [ bgroup "Lit"
      [ bench "1" $ whnf (lookup ["1"]) doubleLit
      , bench "2" $ whnf (lookup ["21"]) doubleLit
      , bench "3" $ whnf (lookup ["41"]) doubleLit
      , bench "4" $ whnf (lookup ["61"]) doubleLit
      , bench "4" $ whnf (lookup ["81"]) doubleLit
      ]
    , bgroup "Pred"
      [ bench "1" $ whnf (lookup ["1"]) doubleAtto
      , bench "2" $ whnf (lookup ["21"]) doubleAtto
      , bench "3" $ whnf (lookup ["41"]) doubleAtto
      , bench "4" $ whnf (lookup ["61"]) doubleAtto
      , bench "4" $ whnf (lookup ["81"]) doubleAtto
      ]
    ]
  ]
