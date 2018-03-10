{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Lib
    ( RTree(..),
      genTree
    ) where
import Control.Monad.Random.Class
import Control.Monad.Random.Lazy
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import Diagrams.Prelude hiding (Empty)
import Data.Either

--GHCi is spitting out some scary Typeable stuff when I infer tree100 but both SVG and Rasterific use Double as their number type so I'm just requiring that for now
--RTree means "randomizable tree" I guess.
class RTree a where
  randTree :: MonadRandom m => m a
  weight :: a -> Int
  depth :: a -> Int
  diagram :: (Renderable (Path V2 Double) b) => a -> QDiagram b V2 Double Any

instance RTree (BTree ()) where
  randTree = randTreeB
  weight = length
  depth Empty = 0
  depth (BNode _ a b)=1+max (depth a) (depth b)
  diagram = treeDia

instance RTree (Tree ()) where
  randTree = randTreeP
  weight = length
  depth = foldTree algebra
    where algebra _ children = maximum $ 0:map (1+) children
  diagram = treeDiaTP

-- This is definitely not a good way to do this but my attempts to do existential types correctly here failed miserably so this is what you get.
instance (RTree a,RTree b) => RTree (Either a b) where
  randTree = getRandom >>= \x->if x then (Left <$> randTree) else (Right <$> randTree)
  weight = either weight weight
  depth = either depth depth
  diagram = either diagram diagram
  
  

goldilocks x = (x > 100) && (x < 10000)

randTreeB :: MonadRandom m => m (BTree ())
randTreeB = join $ uniform [BNode () <$> randTreeB <*> randTreeB,pure Empty]

trees = sequence (repeat randTree)


-- This runs Knuth's algorithm to generate Poisson random numbers with expected value 1.
randTreeP :: MonadRandom m => m (Tree ())
randTreeP = unfoldTreeM (const poissonC) (0.0 :: Double)
  where poissonC = (,) () . tail . takeWhile (>1) . scanl (*) (exp 1.0) <$> getRandomRs (0.0,1.0)

treesP = sequence (repeat randTreeP)

-- thanks to Brent Yorgey for the tree-drawing algorithms.
drawT = maybe mempty (renderTree (const (circle 0.1 # fc red # lc red)) (~~))
      . symmLayoutBin' (with & slVSep .~ 0.5)

drawTP = renderTree (const (circle 0.1 # fc blue # lc blue)) (~~)
       . symmLayout' (with & slVSep .~ 0.5)

treeDia t = drawT t # centerXY # pad 1.1 # bg white

treeDiaTP t = drawTP t # centerXY # pad 1.1 # bg white

pullTree = head . filter (goldilocks . weight) <$> evalRandIO trees


treeEither :: IO (Either (BTree ()) (Tree ()))
treeEither = join $ uniform [Left <$> pullTree, Right <$> pullTree]

genTree = treeEither

someFunc :: IO ()
someFunc = putStrLn "someFunc"
