{-# LANGUAGE
    GADTs
  #-}

module Data.Trie.Pred.Unified.Tail
  ( UPTrie (..)
  , suppliment
  , tagUPTrie
  , measureDepthRelative
  , minDepth
  , maxDepth
  , showTail
  , assignLit
  , elem
  , lookup
  , lookupWithL
  , lookupNearestParent
  , lookupThrough
  , firstNonEmpty
  , merge
  , areDisjoint
  , litSingletonTail
  , litExtrudeTail
  , sort
  ) where

import Prelude hiding (lookup, elem, map)
import Data.List.NonEmpty as NE hiding (map, sort, length)
import Data.Semigroup hiding (First (..), Last (..))
import Data.Monoid hiding ((<>))
import Data.Maybe
import Data.Bifunctor
import Data.Functor.Syntax
import Data.STRef
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST

import Test.QuickCheck



data UPTrie t x where
  UMore :: t
        -> Maybe x
        -> [UPTrie t x]
        -> UPTrie t x
  UPred :: t
        -> (t -> Maybe r)
        -> Maybe (r -> x)
        -> [UPTrie t (r -> x)]
        -> UPTrie t x


-- | Given a parser and a chunk, take a trie expecting a result, and
-- possibly return a reduced trie without the expectation.
suppliment :: (t -> Maybe r) -> t -> UPTrie t (r -> x) -> Maybe (UPTrie t x)
suppliment p t xrs = (xrs <~$>) <$> p t

-- | Acts as a default tag value for the node
tagUPTrie :: UPTrie t x -> t
tagUPTrie (UMore t _ _) = t
tagUPTrie (UPred t _ _ _) = t

-- | Measure the depth of a trie, based on the relation of other adjacent depths
measureDepthRelative :: ([Int] -> Int) -> UPTrie t x -> Int
measureDepthRelative f = go 0
  where
    go :: Int -> UPTrie t x -> Int
    go n (UMore _ _ xs) =
      f $ go (n+1) <$> xs
    go n (UPred t p _ xrs) =
      f $ go (n+1) <$> mapMaybe (suppliment p t) xrs

maxDepth :: UPTrie t x -> Int
maxDepth = measureDepthRelative maximum'
  where
    maximum' [] = 1
    maximum' xs = maximum xs

minDepth :: UPTrie t x -> Int
minDepth = measureDepthRelative minimum'
  where
    minimum' [] = 1
    minimum' xs = minimum xs

instance Functor (UPTrie t) where
  fmap = map

map :: (a -> b) -> UPTrie t a -> UPTrie t b
map f (UMore t mx xs) = UMore t (f <$> mx) $ f <$$> xs
map f (UPred t p mrx xrs) = UPred t p (f <.$> mrx) $ f <.$$> xrs

instance Foldable (UPTrie t) where
  foldMap f xs = fromMaybe mempty $ unwrapMonoid $ go $ f <$> xs
    where
      go (UMore _ mx xs') = WrapMonoid mx <> WrapMonoid (foldMap (unwrapMonoid . go) xs')
      go (UPred t p mrx xrs) = WrapMonoid (mrx <*> p t) <> WrapMonoid (mconcat
        (mapMaybe (\z -> (\r -> unwrapMonoid $ go $ z <~$> r) <$> p t) xrs))

instance (Eq t, Eq x) => Eq (UPTrie t x) where
  (UMore s mx xs) == (UMore t my ys) = t == s && mx == my && xs == ys
  (UPred s p mx xs) == (UPred t q my ys) = s == t
                                        && (mx <*> p s) == (my <*> q t)
                                        && (suppliment p s <$> xs) -- same children
                                        == (suppliment q t <$> ys)
  (UMore s mx xs) == (UPred t q my ys) = s == t
                                      && mx == (my <*> q t)
                                      && (Just <$> xs) == (suppliment q t <$> ys)
  (UPred s p mx xs) == (UMore t my ys) = s == t
                                      && (mx <*> p s) == my
                                      && (suppliment p s <$> xs) == (Just <$> ys)

instance Eq t => Semigroup (UPTrie t x) where
  (<>) = merge

-- | Overwrites when similar, leaves untouched when not
merge :: (Eq t) => UPTrie t x -> UPTrie t x -> UPTrie t x
merge xx@(UMore t mx xs) (UMore p my ys)
  | t == p = UMore p (getLast $ Last mx <> Last my) $ sort $ xs ++ ys
  | otherwise = xx
merge xx@(UPred t _ _ _) yy@(UPred p _ _ _)
  | t == p = yy -- predicate children are incompatible
  | otherwise = xx
merge xx@(UMore t _ _) yy@(UPred p _ _ _)
  | t == p = yy -- rightward bias
  | otherwise = xx
merge xx@(UPred t _ _ _) yy@(UMore p _ _)
  | t == p = yy -- rightward bias
  | otherwise = xx

-- | Can only generate literal examples
instance (Arbitrary t, Arbitrary x) => Arbitrary (UPTrie t x) where
  arbitrary = sized go
    where
      go s = do
        t <- arbitrary
        mx <- arbitrary
        xs <- if s <= 1 then return []
                        else scale (subtract 1) arbitrary `suchThat` (\x -> length x < 10)
        return $ UMore t mx xs

showTail :: (Show t) => UPTrie t x -> String
showTail (UMore t _ xs) = "(UMore " ++ show t ++ ") [" ++ concatMap showTail xs ++ "] "
showTail (UPred t _ _ xs) = "(UPred " ++ show t ++ ") [" ++ concatMap showTail xs ++ "] "

-- | Ignores contents
instance (Show t) => Show (UPTrie t x) where
  show = showTail



type Path t = NonEmpty t

-- | Assigns a value to literal constructors
assignLit :: (Eq t) => Path t -> Maybe x -> UPTrie t x -> UPTrie t x
assignLit (t:|ts) mx yy@(UMore p my ys)
  | t == p = if null ts
             then UMore p mx ys
             else UMore p my $ fmap (assignLit (NE.fromList ts) mx) ys
  | otherwise = yy
assignLit _ _ yy = yy


areDisjoint :: (Eq t) => UPTrie t x -> UPTrie t x -> Bool
areDisjoint (UMore t _ _)    (UMore p _ _)    = t /= p
areDisjoint (UPred t _ _ _)  (UPred p _ _ _)  = t /= p
areDisjoint (UPred t _ _ _)  (UMore p _ _)    = t /= p
areDisjoint (UMore t _ _)    (UPred p _ _ _)  = t /= p

elem :: Eq t => Path t -> UPTrie t x -> Bool
elem ts = isJust . lookup ts

lookup :: Eq t => Path t -> UPTrie t x -> Maybe x
lookup (t:|ts) (UMore t' mx xs) = do
  guard (t == t')
  if null ts then mx
             else firstJust $ fmap (lookup $ NE.fromList ts) xs
lookup (t:|ts) (UPred _ p mrx xrs) = do
  r <- p t
  if null ts then mrx <~$> r
             else firstJust (fmap (lookup $ NE.fromList ts) xrs) <~$> r

-- | Apply a transform @f@ to the final path chunk, when matching a literal
-- cell - used for eliminating file extensions in nested-routes.
lookupWithL :: Eq t => (t -> t) -> Path t -> UPTrie t x -> Maybe x
lookupWithL f (t:|ts) (UMore t' mx xs)
  | null ts = do guard (f t == t')
                 mx
  | otherwise = do guard (t == t')
                   firstJust $ fmap (lookupWithL f $ NE.fromList ts) xs
lookupWithL f (t:|ts) (UPred _ p mrx xrs) = do
  r <- p t
  if null ts then mrx <~$> r
             else firstJust (fmap (lookupWithL f $ NE.fromList ts) xrs) <~$> r

lookupNearestParent :: Eq t => Path t -> UPTrie t x -> Maybe x
lookupNearestParent tss@(t:|ts) trie@(UMore t' mx xs) = firstJust
  [ lookup tss trie
  , do guard (t == t')
       firstJust $ fmap (lookupNearestParent $ NE.fromList ts) xs ++ [mx]
  ]
lookupNearestParent tss@(t:|ts) trie@(UPred _ p mrx xrs) = firstJust
  [ lookup tss trie
  , do r <- p t
       firstJust (fmap (lookupNearestParent $ NE.fromList ts) xrs ++ [mrx]) <~$> r
  ]

-- | Return all nodes passed during a lookup
lookupThrough :: Eq t => Path t -> UPTrie t x -> [x]
lookupThrough (t:|ts) (UMore t' mx xs)
  | null ts = maybeToList $ do guard (t == t')
                               mx
  | otherwise = maybeToList mx
             ++ (do guard (t == t')
                    firstNonEmpty $ lookupThrough (NE.fromList ts) <$> xs)
lookupThrough (t:|ts) (UPred _ p mrx xrs) =
  let (l,r) = fromMaybe (Nothing,[]) $ do
                r <- p t
                return $ if null ts
                         then ( mrx <~$> r, [])
                         else ( mrx <~$> r
                              , firstNonEmpty (fmap (lookupThrough $ NE.fromList ts) xrs) <~$> r
                              )
  in maybeToList l ++ r

firstNonEmpty :: [[a]] -> [a]
firstNonEmpty [] = []
firstNonEmpty (x:xs) | null x = firstNonEmpty xs
                     | otherwise = x

-- | Create a singleton trie out of literal constructors
litSingletonTail :: Path t -> x -> UPTrie t x
litSingletonTail (t:|[]) x = UMore t (Just x) []
litSingletonTail (t:|ts) x = UMore t Nothing  [litSingletonTail (NE.fromList ts) x]

-- | Push a trie down with literal constructors
litExtrudeTail :: [t] -> UPTrie t x -> UPTrie t x
litExtrudeTail [] r = r
litExtrudeTail (t:ts) r = UMore t Nothing [litExtrudeTail ts r]


-- | also does a non-deterministic merge - make sure your nodes are disjoint & clean
sort :: (Eq t) => [UPTrie t x] -> [UPTrie t x]
sort = foldr insert' []
  where
    insert' :: (Eq t) => UPTrie t x -> [UPTrie t x] -> [UPTrie t x]
    insert' r [] = [r]
    insert' x@(UMore t _ _) (y@(UMore p _ _):rs)
      | t == p = x : rs
      | otherwise = x : y : rs
    insert' x@(UMore t _ _) (y@(UPred p _ _ _):rs)
      | t == p = x : rs
      | otherwise = x : y : rs
    insert' x@(UPred t _ _ _) (y@(UPred p _ _ _):rs)
      | t == p = x : rs
      | otherwise = x : y : rs
    insert' x@(UPred t _ _ _) (y@(UMore p _ _):rs)
      | t == p = insert' x rs -- Puts @UPred@ at the bottom
      | otherwise = y : insert' x rs


-- * Utilities

firstJust :: [Maybe a] -> Maybe a
firstJust = getFirst . foldMap First
