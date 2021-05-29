{-# LANGUAGE FlexibleContexts #-}

module GameState where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.State.Strict



-- make an alias in case we want more sophisticated errors in the future
type Error = String
mkError :: String -> Error
mkError = id



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



-- validation of piece positions
onBoard :: MonadState GameState m => Pos -> m Bool
onBoard (x, y) = do
  (width, height) <- getDimension
  return $ 0 <= x && x < width && 0 <= y && y < height

posColor :: Pos -> Color
posColor (x, y) = if x `mod` 2 == y `mod` 2 then Black else White


validateState :: (MonadState GameState m, MonadError Error m) =>
  m ()
validateState = do
  board <- getBoard
  stateIsValid <- foldl foldFunc (pure True) (M.toList board)

  if stateIsValid then
    return ()
  
  else
    throwError $ mkError "state is invalid"

  where 
    foldFunc :: MonadState GameState m => m Bool -> (Piece, Pos) -> m Bool
    foldFunc pureAcc (piece, pos) = do
      acc <- pureAcc
      posOnBoard <- onBoard pos
      let colorMatchesPos = color piece == posColor pos
      return $ acc && posOnBoard && colorMatchesPos



