{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module AppState where


import Control.Monad.State.Strict

import qualified Graphics.Vty as V

import GameState


data Phase = PieceSelection | MoveSelection | OpponentMove | Menu
  deriving (Eq, Ord, Show, Read)

data Button = Play | Exit
  deriving (Eq, Ord, Show, Read)



data AppState = AppState {
  phase :: Phase,
  gameState :: GameState,
  selectedDir :: Direction,
  selectedPos :: Pos,
  player :: Maybe Color,
  msg :: Maybe String, 
  menuButton :: Button
}


initAppState :: AppState
initAppState = AppState {
  phase = Menu,
  gameState = defaultEmptyState,
  selectedDir = TopLeft,
  selectedPos = (0,0),
  --player = Nothing,
  player = Just White,
  msg = Nothing,
  menuButton = Play
}


  

getPhase :: MonadState AppState m => m Phase
getPhase = do
  AppState { phase = ph, .. } <- get
  return ph

getGameState :: MonadState AppState m => m GameState
getGameState = do
  AppState { gameState = gs, .. } <- get
  return gs

getSelectedDir :: MonadState AppState m => m Direction
getSelectedDir = do
  AppState { selectedDir = dir, .. } <- get
  return dir

getSelectedPos :: MonadState AppState m => m Pos
getSelectedPos = do
  AppState { selectedPos = pos, .. } <- get
  return pos

getMsg :: MonadState AppState m => m (Maybe String)
getMsg = do
  AppState { msg = mbMsg, .. } <- get
  return mbMsg

getPlayer :: MonadState AppState m => m (Maybe Color)
getPlayer = do
  AppState { player = pl, .. } <- get
  return pl

getButton :: MonadState AppState m => m Button
getButton = do
  AppState { menuButton = butt, .. } <- get
  return butt


putPhase :: MonadState AppState m => Phase -> m ()
putPhase ph = do
  AppState { phase = _, .. } <- get
  put $ AppState { phase = ph, .. }

putGameState :: MonadState AppState m => GameState -> m ()
putGameState gs = do
  AppState { gameState = _, .. } <- get
  put $ AppState { gameState = gs, .. }


putSelectedDir :: MonadState AppState m => Direction -> m ()
putSelectedDir dir = do
  AppState { selectedDir = _, .. } <- get
  put $ AppState { selectedDir = dir, .. }

putSelectedPos :: MonadState AppState m => Pos -> m ()
putSelectedPos pos = do
  AppState { selectedPos = _, .. } <- get
  put $ AppState { selectedPos = pos, .. }

putMsg :: MonadState AppState m => Maybe String -> m ()
putMsg mbMsg = do
  AppState { msg = _, .. } <- get
  put $ AppState { msg = mbMsg, .. }

putPlayer :: MonadState AppState m => Maybe Color -> m ()
putPlayer mbColor = do
  AppState { player = _, .. } <- get
  put $ AppState { player = mbColor, .. }

putButton :: MonadState AppState m => Button -> m ()
putButton butt = do
  AppState { menuButton = _, .. } <- get
  put $ AppState { menuButton = butt, .. }







unsetMsg :: MonadState AppState m =>  m ()
unsetMsg = putMsg Nothing


neighbourDirButton :: Direction -> V.Key -> Direction
neighbourDirButton TopLeft  V.KRight  = TopRight
neighbourDirButton TopLeft  V.KDown   = BotLeft
neighbourDirButton TopRight V.KLeft   = TopLeft
neighbourDirButton TopRight V.KDown   = BotRight
neighbourDirButton BotLeft  V.KRight  = BotRight
neighbourDirButton BotLeft  V.KUp     = TopLeft
neighbourDirButton BotRight V.KLeft   = BotLeft
neighbourDirButton BotRight V.KUp     = TopRight
neighbourDirButton dir _ = dir

neighbourField :: Pos -> Pos -> V.Key -> Pos
neighbourField dim pos k = let
    (w, h) = dim
    (x, y) = pos
  
  in case k of
    V.KRight  -> (min (w - 1) (x + 1), y)
    V.KLeft   -> (max 0 (x - 1), y)

    -- the y-axis is reversed i.e. 0 is in the top
    V.KDown   -> (x, min (h - 1) (y + 1))
    V.KUp     -> (x, max 0 (y - 1))

    _         -> (x, y)

neighbourButton :: Button -> V.Key -> Button
neighbourButton butt k = case (butt, k) of
  (Play, V.KDown) -> Exit
  (Exit, V.KUp)   -> Play
  _               -> butt
  


selectDir :: MonadState AppState m => V.Key -> m ()
selectDir k = do
  dir <- getSelectedDir
  let newDir = neighbourDirButton dir k
  putSelectedDir newDir

selectPos :: MonadState AppState m => V.Key -> m ()
selectPos k = do
  pos <- getSelectedPos
  gameSt <- getGameState
  
  let dim = dimension gameSt
  let newPos = neighbourField dim pos k

  putSelectedPos newPos

selectButton :: MonadState AppState m => V.Key -> m ()
selectButton k = do
  butt <- getButton
  putButton $ neighbourButton butt k




