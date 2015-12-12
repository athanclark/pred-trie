{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where


import Prelude hiding (lookup)
import           Data.Trie.Pred
import           Data.Trie.Pred.Step (PredStep (..), PredSteps (..))
import           Data.Trie.Class
import           Data.Trie.HashMap (HashMapStep (..))
import qualified Data.HashMap.Lazy as HM
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Attoparsec.Text
import           Criterion.Main
import           Data.Set.Class as Sets


doubleLit :: RootedPredTrie T.Text Double
doubleLit = RootedPredTrie Nothing $ PredTrie
              (HashMapStep $ unUnion $ foldMap (Union . genStep) [1..100])
              (PredSteps [])
  where
    genStep n = HM.singleton (T.pack $ show n) (Just n, Nothing)

doubleAtto :: RootedPredTrie T.Text Double
doubleAtto = RootedPredTrie Nothing $ PredTrie mempty $ PredSteps
  [PredStep "d" (eitherToMaybe . parseOnly double) (Just id) mempty]
  where
    eitherToMaybe (Left _) = Nothing
    eitherToMaybe (Right a) = Just a

deepLit :: RootedPredTrie T.Text Double
deepLit = RootedPredTrie Nothing $ go 10
  where
    go n | n == 0    = PredTrie (HashMapStep HM.empty) (PredSteps [])
         | otherwise = PredTrie (HashMapStep $ HM.singleton (T.pack $ show n) (Just n, Just $ go (n-1)))
                                (PredSteps [])

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
  , bgroup "Lit Deep"
    [ bench "10" $ whnf (lookup ["10"]) deepLit
    , bench "9" $ whnf (lookup ["10","9"]) deepLit
    , bench "8" $ whnf (lookup ["10","9","8"]) deepLit
    , bench "7" $ whnf (lookup ["10","9","8","7"]) deepLit
    , bench "6" $ whnf (lookup ["10","9","8","7","6"]) deepLit
    ]
  ]
