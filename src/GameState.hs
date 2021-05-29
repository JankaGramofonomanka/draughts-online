{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module GameState where

import qualified Data.Map as M

import Control.Monad.Except
import Control.Monad.State.Strict

import Errors ( Error, 
                outOfBoardError, 
                colorMismatchError, 
                pieceNonExistentError
              )


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
  dimension :: (Int, Int),
  numBlacks :: Int,
  numWhites :: Int
} deriving (Ord, Eq, Show, Read)

emptyState :: Int -> Int -> GameState
emptyState w h = GameState {
  board = M.empty,
  dimension = (w, h),
  numBlacks = 0,
  numWhites = 0
}


data Direction
  = TopLeft
  | TopRight
  | BotLeft
  | BotRight

  deriving (Ord, Eq, Show, Read)

type Move = Pos -> Pos

toMove :: Direction -> Move
toMove dir (x, y) = case dir of
  TopLeft   -> (x - 1, y + 1)
  TopRight  -> (x + 1, y + 1)
  BotLeft   -> (x - 1, y - 1)
  BotRight  -> (x + 1, y - 1)



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




getNumBlacks :: MonadState GameState m => m Int
getNumBlacks = do
  state <- get
  return $ numBlacks state

getNumWhites :: MonadState GameState m => m Int
getNumWhites = do
  state <- get
  return $ numWhites state


getNumPieces :: MonadState GameState m => Color -> m Int
getNumPieces Black = getNumBlacks
getNumPieces White = getNumWhites


newBlackPiece :: MonadState GameState m => m Piece
newBlackPiece = do
  GameState { numBlacks = n, .. } <- get
  put $ GameState { numBlacks = n + 1, .. }
  return (Black, n)

newWhitePiece :: MonadState GameState m => m Piece
newWhitePiece = do
  GameState { numWhites = n, .. } <- get
  put $ GameState { numWhites = n + 1, .. }
  return (White, n)

newPiece :: MonadState GameState m => Color -> m Piece
newPiece Black = newBlackPiece
newPiece White = newWhitePiece


-- validation of piece positions
onBoard :: MonadState GameState m => Pos -> m Bool
onBoard (x, y) = do
  (width, height) <- getDimension
  return $ 0 <= x && x < width && 0 <= y && y < height

assertOnBoard :: (MonadState GameState m, MonadError Error m) => Pos -> m ()
assertOnBoard pos = do
  posOnBoard <- onBoard pos
  unless posOnBoard $ throwError outOfBoardError

posColor :: Pos -> Color
posColor (x, y) = if x `mod` 2 == y `mod` 2 then Black else White


validatePiecePlacement :: (MonadState GameState m, MonadError Error m) =>
  Piece -> Pos -> m ()
validatePiecePlacement piece pos = do
  assertOnBoard pos
  
  unless (color piece == posColor pos) $ throwError colorMismatchError



validateState :: (MonadState GameState m, MonadError Error m) =>
  m ()
validateState = do
  board <- getBoard
  mapM_ mapFunc (M.toList board)
  
  where 
    mapFunc (piece, pos) = validatePiecePlacement piece pos


-- piece placement
placePieceUnsafe :: MonadState GameState m => Piece -> Pos -> m ()
placePieceUnsafe piece pos = do
  GameState { board = board, .. } <- get
  let newBoard = M.insert piece pos board
  put $ GameState { board = newBoard, .. }

placeNewPiece :: (MonadState GameState m, MonadError Error m) =>
  Color -> Pos -> m ()
placeNewPiece color pos = do
  piece <- newPiece color
  validatePiecePlacement piece pos
  placePieceUnsafe piece pos


movePiece :: (MonadState GameState m, MonadError Error m) =>
  Piece -> Direction -> m ()
movePiece piece dir = do
  board <- getBoard
  
  unless (M.member piece board) $ throwError pieceNonExistentError

  let move = toMove dir
  let Just pos = M.lookup piece board
  let newPos = move pos

  assertOnBoard newPos
  
  placePieceUnsafe piece newPos




-- game initialization
initState :: Int -> Int -> Int -> GameState
initState w h rows = fromRight $ evalStateT buildState $ emptyState w h where

  -- this should never fail, therefore there `Left` case is undefined
  fromRight (Right x) = x
  
  blackPositions = [ (x, y) | x <- [0..w-1],
                              y <- [0..rows],
                              x `mod` 2 == y `mod` 2 ]

  whitePositions = [ (x, y) | x <- [0..w-1],
                              y <- [h - rows .. h - 1],
                              x `mod` 2 /= y `mod` 2 ]

  buildState :: StateT GameState (Either Error) GameState
  buildState = mapM_ (placeNewPiece Black) blackPositions
            >> mapM_ (placeNewPiece White) whitePositions
            >> get

  
  
  
