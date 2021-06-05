{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module GameState where

import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.Except
import Control.Monad.State.Strict

import Errors ( Error, 
                outOfBoardError, 
                placedOnWhiteError, 
                pieceNonExistentError,
                piecesCollideError,
                opponentPieceError,
                cannotMoveError,
                fieldNotEmptyError,
                opponentMoveError
              )





-- `GameState` data definition ------------------------------------------------
data Color = Black | White deriving (Ord, Eq, Show, Read)
opposite :: Color -> Color
opposite Black = White
opposite White = Black

type Piece = Color

type Pos = (Int, Int)
type Board = M.Map Pos Piece



data GameState = GameState {

  board :: Board,

  -- (width, height)
  dimension :: (Int, Int),

  -- if `lock` = `Just (x, y)`, then only the piece on `(x, y)` can be moved
  lock :: Maybe Pos,

  -- who is currently moving
  mover :: Color

} deriving (Ord, Eq, Show, Read)

emptyState :: Int -> Int -> GameState
emptyState w h = GameState {
  board = M.empty,
  dimension = (w, h),
  lock = Nothing,
  mover = White
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
  TopLeft   -> (x - 1, y - 1)
  TopRight  -> (x + 1, y - 1)
  BotLeft   -> (x - 1, y + 1)
  BotRight  -> (x + 1, y + 1)





-- get functions --------------------------------------------------------------
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

getLock :: MonadState GameState m => m (Maybe Pos)
getLock = do
  state <- get
  return $ lock state

getMover :: MonadState GameState m => m Color
getMover = do
  state <- get
  return $ mover state





getNumPieces :: MonadState GameState m => Color -> m Int
getNumPieces color = do
  board <- getBoard
  return $ length [() | (_, col) <- M.toList board, color == col]


getPiece :: (MonadState GameState m, MonadError Error m) =>
  Color -> Pos -> m Piece
getPiece color pos = do
  
  occopancy <- posOccupancy pos
  case occopancy of
    Nothing -> throwError pieceNonExistentError 
    Just piece -> do
      unless (piece == color) $ throwError opponentPieceError
      return piece

posOccupancy :: MonadState GameState m => Pos -> m (Maybe Color)
posOccupancy pos = do
  board <- getBoard
  return $ M.lookup pos board





-- put / modify functions -----------------------------------------------------
modifyBoard :: MonadState GameState m => (Board -> Board) -> m ()
modifyBoard modFunc = do
  GameState { board = board, .. } <- get
  put $ GameState { board = modFunc board, .. }

putLock :: MonadState GameState m => (Maybe Pos) -> m ()
putLock newLock = do
  GameState { lock = _, .. } <- get
  put $ GameState { lock = newLock, .. }

switchMover :: MonadState GameState m => m ()
switchMover = do
  GameState { mover = color, .. } <- get
  put $ GameState { mover = opposite color, .. }






-- validations and assertions -------------------------------------------------
onBoard :: MonadState GameState m => Pos -> m Bool
onBoard (x, y) = do
  (width, height) <- getDimension
  return $ 0 <= x && x < width && 0 <= y && y < height

assertOnBoard :: (MonadState GameState m, MonadError Error m) => Pos -> m ()
assertOnBoard pos = do
  posOnBoard <- onBoard pos
  unless posOnBoard $ throwError outOfBoardError

-- pieces can be placed only on black (valid) fields
validPos :: Pos -> Bool
validPos (x, y) = x `mod` 2 == y `mod` 2


validatePiecePlacement :: (MonadState GameState m, MonadError Error m) =>
  Pos -> m ()
validatePiecePlacement pos = do
  assertOnBoard pos
  
  unless (validPos pos) $ throwError placedOnWhiteError





validateState :: (MonadState GameState m, MonadError Error m) => m ()
validateState = do
  board <- getBoard
  mapM_ mapFunc (M.toList board)
  
  where 
    mapFunc (pos, piece) = validatePiecePlacement pos


assertEmpty :: (MonadState GameState m, MonadError Error m) => Pos -> m ()
assertEmpty pos = do
  occopancy <- posOccupancy pos
  unless (occopancy == Nothing) $ throwError fieldNotEmptyError


assertCanMove :: (MonadState GameState m, MonadError Error m) => 
  Color -> Pos -> m ()
assertCanMove color pos = do
  mover <- getMover
  unless (mover == color) $ throwError opponentMoveError

  lock <- getLock
  
  case lock of
    Nothing -> return ()
    Just lockedPos -> unless (lockedPos == pos) $ throwError cannotMoveError





-- piece placement ------------------------------------------------------------
placePieceUnsafe :: MonadState GameState m => Piece -> Pos -> m ()
placePieceUnsafe piece pos = modifyBoard $ M.insert pos piece

removePieceUnsafe :: MonadState GameState m => Pos -> m ()
removePieceUnsafe pos = modifyBoard $ M.delete pos


placeNewPiece :: (MonadState GameState m, MonadError Error m) =>
  Piece -> Pos -> m ()
placeNewPiece piece pos = do
  validatePiecePlacement pos
  occopancy <- posOccupancy pos
  unless (occopancy == Nothing) $ throwError piecesCollideError
  placePieceUnsafe piece pos





-- lock / unlock pieces -------------------------------------------------------
lockPieceUnsafe :: MonadState GameState m => Pos -> m ()
lockPieceUnsafe pos = putLock $ Just pos


lockPiece :: (MonadState GameState m, MonadError Error m) => 
  Color -> Pos -> m ()
lockPiece color pos = do
  occopancy <- posOccupancy pos
  when (occopancy == Nothing) $ throwError pieceNonExistentError
  when (occopancy == Just (opposite color)) $ throwError opponentPieceError 
  lockPieceUnsafe pos

unlock :: MonadState GameState m => m ()
unlock = putLock Nothing





-- piece movement -------------------------------------------------------------

-- move piece and return the color that will move next
movePiece :: (MonadState GameState m, MonadError Error m) =>
  Color -> Pos -> Direction -> m ()
movePiece color pos dir = do

  assertCanMove color pos
  
  let move = toMove dir
  let newPos = move pos


  piece <- getPiece color pos
  validatePiecePlacement newPos


  occopancy <- posOccupancy newPos
  case occopancy of
    
    Nothing -> do
      removePieceUnsafe pos
      placePieceUnsafe color newPos

      unlock

      switchMover


    Just piece -> do
      when (piece == color) $ throwError piecesCollideError
      
      let newNewPos = move newPos
      validatePiecePlacement newNewPos
      assertEmpty newNewPos

      removePieceUnsafe pos
      removePieceUnsafe newPos
      placePieceUnsafe color newNewPos
      
      lockPiece color newNewPos





-- game initialization --------------------------------------------------------
initState :: Int -> Int -> Int -> GameState
initState w h rows = fromRight $ evalStateT buildState $ emptyState w h where

  -- this should never fail, therefore there `Left` case is undefined
  fromRight (Right x) = x
  
  blackPositions = [ (x, y) | x <- [0..w-1],
                              y <- [0..rows],
                              x `mod` 2 == y `mod` 2 ]

  whitePositions = [ (x, y) | x <- [0..w-1],
                              y <- [h - rows .. h - 1],
                              x `mod` 2 == y `mod` 2 ]

  buildState :: StateT GameState (Either Error) GameState
  buildState = mapM_ (placeNewPiece Black) blackPositions
            >> mapM_ (placeNewPiece White) whitePositions
            >> get


defaultInitState :: GameState
defaultInitState = initState 8 8 3

