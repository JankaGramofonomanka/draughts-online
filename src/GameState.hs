module GameState where

import qualified Data.Map as M

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

