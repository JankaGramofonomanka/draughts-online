{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module AppState where


import Control.Monad.State


import Brick
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
  gameState :: GameState
}


initAppState :: AppState
initAppState = AppState {
  phase = MoveSelection, 
  gameState = defaultInitState
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





handleEvent appState event = case event of
  (VtyEvent (V.EvKey V.KEsc []))    -> halt appState 
  (VtyEvent (V.EvKey V.KEnter []))  -> continue $ execState swichPhase appState 
  _                                 -> continue appState 
