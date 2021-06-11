{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module AppState where


import Control.Monad.State


import Brick hiding (Direction)
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty as V


import GameState


data Phase = PieceSelection | MoveSelection | Waiting
  deriving (Eq, Ord, Show, Read)

nextPhase :: Phase -> Phase
nextPhase ph = case ph of
  PieceSelection  -> MoveSelection
  MoveSelection   -> Waiting
  Waiting         -> PieceSelection


data AppState = AppState {
  phase :: Phase,
  gameState :: GameState,
  selectedDir :: Direction
}


initAppState :: AppState
initAppState = AppState {
  phase = MoveSelection, 
  gameState = defaultInitState,
  selectedDir = TopLeft
}


getPhase :: MonadState AppState m => m Phase
getPhase = do
  AppState { phase = ph, .. } <- get
  return ph

putPhase :: MonadState AppState m => Phase -> m ()
putPhase ph = do
  AppState { phase = _, .. } <- get
  put $ AppState { phase = ph, .. }


swichPhase :: MonadState AppState m => m ()
swichPhase = do
  ph <- getPhase
  putPhase $ nextPhase ph




neighbourDirButton :: Direction -> Key -> Direction
neighbourDirButton TopLeft  V.KRight  = TopRight
neighbourDirButton TopLeft  V.KDown   = BotLeft
neighbourDirButton TopRight V.KLeft   = TopLeft
neighbourDirButton TopRight V.KDown   = BotRight
neighbourDirButton BotLeft  V.KRight  = BotRight
neighbourDirButton BotLeft  V.KUp     = TopLeft
neighbourDirButton BotRight V.KLeft   = BotLeft
neighbourDirButton BotRight V.KUp     = TopRight
neighbourDirButton dir _ = dir



handleArrow :: MonadState AppState m => Key -> m ()
handleArrow k = do
  AppState { selectedDir = dir, .. } <- get
  let newDir = neighbourDirButton dir k
  put $ AppState { selectedDir = newDir, .. }



handleEvent :: AppState -> BrickEvent n e -> EventM n1 (Next AppState)
handleEvent appState (VtyEvent (V.EvKey k [])) = if isArrow k then
    continue $ execState (handleArrow k) appState
    
  else case k of
    V.KEsc    -> halt appState 
    V.KEnter  -> continue $ execState swichPhase appState 

    _         -> continue appState

handleEvent appState _ = continue appState

isArrow :: Key -> Bool
isArrow k = case k of
  V.KLeft   -> True
  V.KRight  -> True
  V.KUp     -> True
  V.KDown   -> True
  _         -> False







