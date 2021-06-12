{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module AppState where


import Data.Maybe
import Control.Monad.State.Strict
import Control.Exception


import Brick hiding (Direction)
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V


import Data.Aeson
import qualified Network.Wreq as Rq
import Control.Lens
import Network.HTTP.Client
import Data.ByteString.UTF8

import GameState
import DataFormatting


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
  selectedDir :: Direction,
  selectedPos :: Pos,
  player :: Maybe Color,
  msg :: Maybe String
}


initAppState :: AppState
initAppState = AppState {
  phase = PieceSelection, 
  gameState = defaultInitState,
  selectedDir = TopLeft,
  selectedPos = (0,0),
  player = Just White,
  msg = Nothing 
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



mkMove :: (MonadState AppState m, MonadIO m) => m ()
mkMove = do
  AppState {
    gameState = gameSt,
    player = color,
    selectedPos = pos, 
    selectedDir = dir,
    .. } <- get

  let rqBody = toJSON $ MV (fromJust color, pos, dir)

  resp <- liftIO $ Rq.put "http://127.0.0.1:11350/move" rqBody

  jsonResp <- liftIO $ Rq.asJSON resp
  let newGameSt = jsonResp ^. Rq.responseBody
  
  put $ AppState {
    gameState = newGameSt, 
    player = color,
    selectedPos = pos, 
    selectedDir = dir,
    .. }
  

setMsg :: MonadState AppState m =>  HttpException -> m ()
setMsg e = case e of
  
  HttpExceptionRequest _ (StatusCodeException _ txt) -> 
    actuallySetMsg $ toString txt
  _ -> actuallySetMsg " unknown exception"


  where
    actuallySetMsg txt = do
      AppState { msg = msg, .. } <- get
      let newMsg = Just $ "Error: " ++ txt
      put $ AppState { msg = newMsg, .. }


unsetMsg :: MonadState AppState m =>  m ()
unsetMsg = do
  AppState { msg = msg, .. } <- get
  put $ AppState { msg = Nothing, .. }


handleEnter :: AppState ->  EventM n1 (Next AppState)
handleEnter appState = case phase appState of
  MoveSelection -> suspendAndResume $ catch x handler

  _ -> continue $ execState swichPhase appState 

  where
    x = execStateT (unsetMsg >> mkMove >> swichPhase) appState

    handler :: HttpException -> IO AppState
    handler e = return $ execState (setMsg e >> swichPhase) appState



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



selectDir :: MonadState AppState m => V.Key -> m ()
selectDir k = do
  AppState { selectedDir = dir, .. } <- get
  let newDir = neighbourDirButton dir k
  put $ AppState { selectedDir = newDir, .. }

selectPos :: MonadState AppState m => V.Key -> m ()
selectPos k = do
  AppState { selectedPos = pos, gameState = gameSt, .. } <- get
  
  let dim = dimension gameSt

  let newPos = neighbourField dim pos k

  put $ AppState { selectedPos = newPos, gameState = gameSt, .. }
  

handleArrow :: MonadState AppState m => V.Key -> m ()
handleArrow k = do
  ph <- getPhase

  case ph of
    MoveSelection   -> selectDir k
    PieceSelection  -> selectPos k
    _               -> return ()



swichPlayer :: MonadState AppState m => m ()
swichPlayer = do
  AppState { player = mColor, .. } <- get
  case mColor of
    Nothing -> return ()
    Just color -> put $ AppState { player = Just $ opposite color, .. }

handleEvent :: AppState -> BrickEvent n e -> EventM n1 (Next AppState)
handleEvent appState (VtyEvent (V.EvKey k [])) = if isArrow k then
    continue $ execState (handleArrow k) appState
    
  else case k of
    V.KEsc    -> halt appState 
    V.KEnter  -> handleEnter appState 

    -- cheat for the purpose of testing
    V.KChar 'c' -> continue $ execState swichPlayer appState

    _         -> continue appState

handleEvent appState _ = continue appState

isArrow :: V.Key -> Bool
isArrow k = case k of
  V.KLeft   -> True
  V.KRight  -> True
  V.KUp     -> True
  V.KDown   -> True
  _         -> False







