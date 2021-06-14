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
  x2  <- frequency [ (1, pure i) | i <- [0..3] ]
  y2  <- frequency [ (1, pure i) | i <- [0..3] ]
  r   <- frequency [ (1, pure r) | r <- [0, 1] ]
  
  let x = 2 * x2 + r
  let y = 2 * y2 + r
  return (x, y)


-- `instance Arbitrary Board` would be overlapping
arbitraryBoardN :: Int -> Int -> Gen Board
arbitraryBoardN 0 0 = pure M.empty
arbitraryBoardN 0 numW = do
  pos <- arbitraryPos
  board <- arbitraryBoardN 0 (numW - 1)
  return $ M.insert pos White board

arbitraryBoardN numB 0 = do
  pos <- arbitraryPos
  board <- arbitraryBoardN (numB - 1) 0
  return $ M.insert pos Black board

arbitraryBoardN numB numW = do
  pos <- arbitraryPos
  color <- arbitrary

  let (newNB, newNW) = newNums color
  board <- arbitraryBoardN newNB newNW

  return $ M.insert pos color board

  where
    newNums c = case c of
      White -> (numB, numW - 1)  
      Black -> (numB - 1, numW)

arbitraryBoard :: Gen Board 
arbitraryBoard = do
  numB <- frequency [ (freq i, pure i) | i <- [0..16] ]
  numW <- frequency [ (freq i, pure i) | i <- [0..16] ]
  
  arbitraryBoardN numB numW

  where 
    
    freq i
      | i < 0     = 0
      | i <= 4    = 1
      | i <= 8    = 2
      | i <= 12   = 3
      | i <= 16   = 4
      | otherwise = 0
  


instance Arbitrary GameState where
  arbitrary = do

    board <- arbitraryBoard
    dimension <- pure (8, 8)
    combo <- frequency [(1, pure True), (10, pure False)]
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
        n <- frequency [ (1, pure i) | i <- [1..3] ]
        excl <- replicateM n arbitrary

        return (Just pos, nub excl)

instance Arbitrary Direction where
  arbitrary = frequency [ (1, pure dir) | dir <- allDirections ]


instance Arbitrary MoveInfo where
  arbitrary = do
    color <- arbitrary
    pos <- arbitraryPos
    dir <- arbitrary 
    return $ MV (color, pos, dir)


