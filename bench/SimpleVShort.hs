module Main where


import           Data.Trie.Pred.FastUnified
import qualified Data.Trie.Pred.FastUnified as FU
import           Data.Trie.Pred.NormUnified
import qualified Data.Trie.Pred.NormUnified as NU
import Criterion.Main
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE


tdFU = FUMore "foo" Nothing $ NE.fromList
         [ FURest ("bar":|["baz","qux"]) 1
         , FUMore "tro" (Just 2) $ NE.fromList
             [ FUMore "zja" (Just 3) $ NE.fromList
                 [ FURest ("hda":|["jes","kuq"]) 4 ]
             , FURest ("end":|["orp","vag"]) 5
             ]
         , FURest ("dic":|["but","gea"]) 6
         ]

tdNU = NUMore "foo" Nothing
         [ NUMore "bar" Nothing
             [ NUMore "baz" Nothing
                 [ NUMore "qux" (Just 1) []
                 ]
             ]
         , NUMore "tro" (Just 2)
             [ NUMore "zja" (Just 3)
                 [ NUMore "hda" Nothing
                     [ NUMore "jes" Nothing
                         [ NUMore "kuq" (Just 4) []
                         ]
                     ]
                 ]
             , NUMore "end" Nothing
                 [ NUMore "orp" Nothing
                     [ NUMore "vag" (Just 5) []
                     ]
                 ]
             ]
         , NUMore "dic" Nothing
             [ NUMore "but" Nothing
                 [ NUMore "gea" (Just 6) []
                 ]
             ]
         ]



main = defaultMain
  [ bgroup "Fast" [ bench "foobarbaz"       $ whnf (FU.lookup $ "foo":|["bar","baz"]) tdFU
                  , bench "footrozjahdajes" $ whnf (FU.lookup $ "foo":|["tro","zja","hda","jes"]) tdFU
                  , bench "footroendorpvag" $ whnf (FU.lookup $ "foo":|["tro","end","orp","vag"]) tdFU
                  , bench "foodicbutgea"    $ whnf (FU.lookup $ "foo":|["dic","but","gea"]) tdFU
                  ]

  , bgroup "Norm" [ bench "foobarbaz"       $ whnf (NU.lookup $ "foo":|["bar","baz"]) tdNU
                  , bench "footrozjahdajes" $ whnf (NU.lookup $ "foo":|["tro","zja","hda","jes"]) tdNU
                  , bench "footroendorpvag" $ whnf (NU.lookup $ "foo":|["tro","end","orp","vag"]) tdNU
                  , bench "foodicbutgea"    $ whnf (NU.lookup $ "foo":|["dic","but","gea"]) tdNU
                  ]
  ]
