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


data Phase = PieceSelection | MoveSelection | OpponentMove | Menu
  deriving (Eq, Ord, Show, Read)

data Button = Play | Exit
  deriving (Eq, Ord, Show, Read)

nextPhase :: Phase -> Phase
nextPhase ph = case ph of
  PieceSelection  -> MoveSelection
  MoveSelection   -> OpponentMove
  OpponentMove    -> PieceSelection


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



swichPhase :: MonadState AppState m => m ()
swichPhase = do
  ph <- getPhase
  putPhase $ nextPhase ph



mkMove :: (MonadState AppState m, MonadIO m) => m ()
mkMove = do

  gameSt <- getGameState
  color <- getPlayer
  pos <- getSelectedPos
  dir <- getSelectedDir

  let rqBody = toJSON $ MV (fromJust color, pos, dir)

  resp <- liftIO $ Rq.put "http://127.0.0.1:11350/move" rqBody

  jsonResp <- liftIO $ Rq.asJSON resp
  let newGameSt = jsonResp ^. Rq.responseBody
  
  putGameState newGameSt

  case lock newGameSt of
    Nothing -> putPhase OpponentMove

    Just locked -> do
      putSelectedPos locked
      putPhase MoveSelection


requestGameState :: (MonadState AppState m, MonadIO m) => m ()
requestGameState = do

  resp <- liftIO $ Rq.get "http://127.0.0.1:11350/state"
  jsonResp <- liftIO $ Rq.asJSON resp
  let newGameSt = jsonResp ^. Rq.responseBody

  putGameState newGameSt
  putPhase PieceSelection
  
  

setMsg :: MonadState AppState m =>  HttpException -> m ()
setMsg e = case e of
  
  HttpExceptionRequest _ (StatusCodeException _ txt) -> 
    putMsg $ Just $ "WRONG: " ++ toString txt
  
  _ -> putMsg $ Just "Error: unknown exception"


unsetMsg :: MonadState AppState m =>  m ()
unsetMsg = putMsg Nothing


handleEnter :: AppState -> EventM n1 (Next AppState)
handleEnter appState = case phase appState of
  PieceSelection  -> continue $ execState (putPhase MoveSelection) appState 
  MoveSelection   -> suspendAndResume $ catch execMove handler
  Menu            -> execMenuButton appState
  
  _               -> continue appState 

  where
    execMove = execStateT (unsetMsg >> mkMove) appState

    handler :: HttpException -> IO AppState
    handler e = return $ 
      execState (setMsg e >> resetMove) appState

resetMove :: MonadState AppState m => m ()
resetMove = do
  gameSt <- getGameState
  case lock gameSt of
    Nothing -> putPhase PieceSelection

    Just locked -> do
      putSelectedPos locked
      putPhase MoveSelection

execMenuButton :: AppState -> EventM n1 (Next AppState)
execMenuButton appState = let
    butt = menuButton appState

  in case butt of

    Exit -> halt appState
    Play -> suspendAndResume $ catch execView handler

    where
      execView = execStateT requestGameState appState

      handler :: HttpException -> IO AppState
      handler e = return $ execState (setMsg e) appState




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

handleArrow :: MonadState AppState m => V.Key -> m ()
handleArrow k = do
  ph <- getPhase

  case ph of
    Menu            -> selectButton k
    MoveSelection   -> selectDir k
    PieceSelection  -> selectPos k
    _               -> return ()



swichPlayer :: MonadState AppState m => m ()
swichPlayer = do
  mColor <- getPlayer
  putPlayer $ opposite <$> mColor
  putPhase PieceSelection
  
handleEvent :: AppState -> BrickEvent n e -> EventM n1 (Next AppState)
handleEvent appState (VtyEvent (V.EvKey k [])) = if isArrow k then
    continue $ execState (handleArrow k) appState
    
  else case k of
    V.KEsc      -> continue $ execState (putPhase Menu) appState
    V.KEnter    -> handleEnter appState 

    -- cheat for the purpose of testing
    V.KChar 'c' -> continue $ execState swichPlayer appState

    _           -> continue appState

handleEvent appState _ = continue appState

isArrow :: V.Key -> Bool
isArrow k = case k of
  V.KLeft   -> True
  V.KRight  -> True
  V.KUp     -> True
  V.KDown   -> True
  _         -> False







