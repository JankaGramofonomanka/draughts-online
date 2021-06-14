{-# LANGUAGE FlexibleInstances #-}

module Testing.ArbitraryInstances where

import Data.List
import qualified Data.Map as M

import Control.Monad

import Test.QuickCheck


import GameState
import DataFormatting


instance Arbitrary Color where
  arbitrary = frequency [(1, pure Black), (1, pure White)]

-- `instance Arbitrary Pos` would be overlapping
arbitraryPos :: Gen Pos
arbitraryPos = do
  x2 <- frequency [(1, pure i) | i <- [0..3]]
  y2 <- frequency [(1, pure i) | i <- [0..3]]
  r <- frequency [(1, pure r) | r <- [0, 1]]
  --(xR, yR) <- frequency [ (5, pure (r, r)),
  --                        (1, pure (r, 0)),
  --                        (1, pure (0, r))
  --                      ]
  let xR = r
  let yR = r
  
  let x = 2 * x2 + xR
  let y = 2 * y2 + yR
  return (x, y)


-- `instance Arbitrary Board` would be overlapping
arbitraryBoard :: Gen Board
arbitraryBoard = do
  pos <- arbitraryPos
  color <- arbitrary
  board <- arbitraryBoard

  frequency [(1, pure M.empty), (5, pure $ M.insert pos color board)]


instance Arbitrary GameState where
  arbitrary = do

    board <- arbitraryBoard
    dimension <- pure (8, 8)
    combo <- arbitrary
    (lock, excl) <- if combo then arbitraryLock else pure (Nothing, [])

    mover <- arbitrary
    joined <- arbitrary
    winner <- arbitrary

    return $ GameState {
        board               = board,
        dimension           = dimension,
        lock                = lock,
        excludedDirections  = excl,
        mover               = mover,
        joined              = joined,
        winner              = winner
    }

    where

      arbitraryLock :: Gen (Maybe Pos, [Direction])
      arbitraryLock = do
        pos <- arbitraryPos
        n <- frequency [(1, pure i) | i <- [1..3]]
        excl <- replicateM n arbitrary

        return (Just pos, nub excl)

instance Arbitrary Direction where
  arbitrary = frequency [(1, pure dir) | dir <- allDirections]


instance Arbitrary MoveInfo where
  arbitrary = do
    color <- arbitrary
    pos <- arbitraryPos
    dir <- arbitrary 
    return $ MV (color, pos, dir)


