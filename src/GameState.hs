{-# LANGUAGE FlexibleContexts #-}

module GameState where

import qualified Data.Map as M

import Control.Monad.State.Strict


-- `GameState` data definition
data Color = Black | White deriving (Ord, Eq, Show, Read)
type Piece = (Color, Int)
color :: Piece -> Color
color = fst
num :: Piece -> Int
num = snd

type Pos = (Int, Int)
type Board = M.Map Piece Pos

data GameState = GameState {
  board :: Board,
  dimension :: (Int, Int)
} deriving (Ord, Eq, Show, Read)

emptyState :: Int -> Int -> GameState
emptyState w h = GameState {board = M.empty, dimension = (w, h)}



-- get functions
getBoard :: MonadState GameState m => m Board
getBoard = do
  state <- get
  return $ board state  


getDimension :: MonadState GameState m => m (Int, Int)
getDimension = do
  state <- get
  return $ dimension state

getWidth :: MonadState GameState m => m Int
getWidth = do
  (width, height) <- getDimension
  return width

getHeight :: MonadState GameState m => m Int
getHeight = do
  (width, height) <- getDimension
  return height





