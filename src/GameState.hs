{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module GameState where

import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.Except
import Control.Monad.State.Strict

import Errors


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

  excludedDirections :: [Direction],

  -- who is currently moving
  mover :: Color,

  -- who joined
  joined :: (Bool, Bool),

  -- who won
  winner :: Maybe Color

} deriving (Ord, Eq, Show, Read)

emptyState :: Int -> Int -> GameState
emptyState w h = GameState {
  board = M.empty,
  dimension = (w, h),
  lock = Nothing,
  excludedDirections = [],
  mover = White, 
  joined = (False, False),
  winner = Nothing
}

defaultEmptyState = emptyState 8 8


data Direction
  = TopLeft
  | TopRight
  | BotLeft
  | BotRight

  deriving (Ord, Eq, Show, Read)

allDirections :: [Direction]
allDirections = [TopLeft, TopRight, BotLeft, BotRight]

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

getExcludedDirs :: MonadState GameState m => m [Direction]
getExcludedDirs = do
  state <- get
  return $ excludedDirections state

getMover :: MonadState GameState m => m Color
getMover = do
  state <- get
  return $ mover state

getJoined :: MonadState GameState m => m (Bool, Bool)
getJoined = do
  state <- get
  return $ joined state

getWinner :: MonadState GameState m => m (Maybe Color)
getWinner = do
  state <- get
  return $ winner state





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

swichMover :: MonadState GameState m => m ()
swichMover = do
  GameState { mover = color, .. } <- get
  put $ GameState { mover = opposite color, .. }

putJoined :: MonadState GameState m => (Bool, Bool) -> m ()
putJoined jnd = do
  GameState { joined = _, .. } <- get
  put $ GameState { joined = jnd, .. }

putExcludedDirs :: MonadState GameState m => [Direction] -> m ()
putExcludedDirs dirs = do
  GameState { excludedDirections = _, .. } <- get
  put $ GameState { excludedDirections = dirs, .. }

putWinner :: MonadState GameState m => Maybe Color -> m ()
putWinner mbColor = do
  GameState { winner = _, .. } <- get
  put $ GameState { winner = mbColor, .. }




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
  Color -> Pos -> Direction -> m ()
assertCanMove color pos dir = do
  mover <- getMover
  unless (mover == color) $ throwError opponentMoveError

  excluded <- getExcludedDirs
  when (dir `elem` excluded) $ throwError lockedDirectionError

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





-- combo handling -------------------------------------------------------------
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

makeDirsExclusive :: MonadState GameState m => [Direction] -> m ()
makeDirsExclusive dirs = do

  let excluded = filter (`notElem` dirs) allDirections

  putExcludedDirs excluded

unlockDirs :: MonadState GameState m => m ()
unlockDirs = putExcludedDirs []





dirsWhereScorePossible :: (MonadState GameState m) => 
  Color -> Pos -> m [Direction]
dirsWhereScorePossible color pos = 
  filterM (scorePossible color pos) allDirections

hasOpponentPiece :: MonadState GameState m =>
  Color -> Pos -> Direction -> m Bool
hasOpponentPiece color pos dir = do
  let move = toMove dir
  let neighbourPos = move pos

  occupancy <- posOccupancy neighbourPos

  case occupancy of
    Nothing -> return False
    Just col -> return $ col == opposite color

scorePossible :: (MonadState GameState m) =>
  Color -> Pos -> Direction -> m Bool
scorePossible color pos dir = do
  let move = toMove dir
  let destination = (move . move) pos

  validDestination <- onBoard destination
  sthToBeScored <- hasOpponentPiece color pos dir
  occupancy <- posOccupancy destination
  
  return $ validDestination && sthToBeScored && occupancy == Nothing



-- piece movement -------------------------------------------------------------

-- move piece and return the color that will move next
movePiece :: (MonadState GameState m, MonadError Error m) =>
  Color -> Pos -> Direction -> m ()
movePiece color pos dir = do

  assertCanMove color pos dir
  unlockDirs
  
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

      swichMover


    Just piece -> do
      when (piece == color) $ throwError piecesCollideError
      
      let newNewPos = move newPos
      validatePiecePlacement newNewPos
      assertEmpty newNewPos

      removePieceUnsafe pos
      removePieceUnsafe newPos
      placePieceUnsafe color newNewPos

      scoreDirs <- dirsWhereScorePossible color newNewPos
      if null scoreDirs then do
        swichMover
        unlock
      
      else do
        makeDirsExclusive scoreDirs
        lockPiece color newNewPos



checkWinner :: MonadState GameState m => m ()
checkWinner = do
  numBlacks <- getNumPieces Black
  numWhites <- getNumPieces White
  
  if numBlacks == 0 then do
    putWinner $ Just White
    putJoined (False, False)
  
  else if numWhites == 0 then do
    putWinner $ Just Black
    putJoined (False, False)

  else
    return ()



-- game initialization --------------------------------------------------------
initState :: Int -> Int -> Int -> GameState
initState w h rows = fromRight $ evalStateT buildState $ emptyState w h where

  -- this should never fail, therefore there `Left` case is undefined
  fromRight (Right x) = x
  
  blackPositions = [ (x, y) | x <- [0..w-1],
                              y <- [0 .. rows - 1],
                              validPos (x, y) ]

  whitePositions = [ (x, y) | x <- [0..w-1],
                              y <- [h - rows .. h - 1],
                              validPos (x, y) ]

  buildState :: StateT GameState (Either Error) GameState
  buildState = mapM_ (placeNewPiece Black) blackPositions
            >> mapM_ (placeNewPiece White) whitePositions
            >> get


defaultInitState :: GameState
defaultInitState = initState 8 8 3

