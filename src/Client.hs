{-# LANGUAGE FlexibleContexts #-}

module Client where

import Data.Maybe
import Control.Monad.State.Strict
import Control.Exception


import Brick hiding (Direction)
import qualified Graphics.Vty as V


import Data.Aeson
import qualified Network.Wreq as Rq
import Control.Lens
import Network.HTTP.Client
import Data.ByteString.UTF8

import GameState
import DataFormatting
import AppState


checkEndGame :: MonadState AppState m => m ()
checkEndGame = do
  gameSt <- getGameState

  case winner gameSt of
    Nothing -> return ()
    Just color -> setWinMsg color

setWinMsg :: MonadState AppState m => Color -> m ()
setWinMsg color = do
  mbPlayer <- getPlayer
  case mbPlayer of
    Nothing -> putMsg $ Just $ "The winner is: " ++ show color
    Just col -> putMsg $ Just msg 
      where
        msg = if color == col then "YOU WON!!!" else "GAME OVER!"


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
  checkEndGame

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
  checkEndGame


joinGame :: (MonadState AppState m, MonadIO m) => m ()
joinGame = do

  -- something to put in the request body
  let rqBody = toJSON ()

  resp <- liftIO $ Rq.post "http://127.0.0.1:11350/join" rqBody
  jsonResp <- liftIO $ Rq.asJSON resp
  let player = jsonResp ^. Rq.responseBody

  putPlayer $ Just player
  

joinIfNecessary :: (MonadState AppState m, MonadIO m) => m ()
joinIfNecessary = do
  mbPlayer <- getPlayer

  when (mbPlayer == Nothing) joinGame


updatePhase :: MonadState AppState m => m ()
updatePhase = do
  mbPlayer <- getPlayer

  case mbPlayer of
    Nothing -> putPhase Watching
    
    Just player -> do  
        gameSt <- getGameState

        if mover gameSt == player then
          putPhase PieceSelection
        
        else
          putPhase OpponentMove
          



setErrMsg :: MonadState AppState m => HttpException -> m ()
setErrMsg e = case e of
  
  HttpExceptionRequest _ (StatusCodeException _ txt) -> 
    putMsg $ Just $ "WRONG: " ++ toString txt
  
  _ -> putMsg $ Just "Error: unknown exception"


handleEnter :: AppState -> EventM n1 (Next AppState)
handleEnter appState = case phase appState of
  PieceSelection  -> continue $ execState (putPhase MoveSelection) appState 
  MoveSelection   -> execMove appState
  Menu            -> execMenuButton appState
  
  _               -> continue appState 




execMove :: AppState -> EventM n (Next AppState)
execMove appState = liftIO (catch exec handler) >>= continue
  
  where

    exec = execStateT (unsetMsg >> mkMove) appState

    handler :: HttpException -> IO AppState
    handler e = return $ 
      execState (setErrMsg e >> resetMove) appState

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

    Exit  -> halt appState
    Watch -> liftIO (catchExec execWatch) >>= continue
    Play  -> liftIO (catchExec execPlay)  >>= continue

    where
      catchExec exec = catch (execStateT exec appState) handler
      execWatch = requestGameState >> updatePhase
      execPlay = joinIfNecessary >> requestGameState >> updatePhase

      handler :: HttpException -> IO AppState
      handler e = return $ execState (setErrMsg e) appState





handleArrow :: MonadState AppState m => V.Key -> m ()
handleArrow k = do
  ph <- getPhase

  case ph of
    Menu            -> selectButton k
    MoveSelection   -> selectDir k
    PieceSelection  -> selectPos k
    OpponentMove    -> selectPos k
    _               -> return ()


-- a cheat to be removed
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

handleEvent appState (AppEvent e) = 
  liftIO (execStateT execView appState) >>= continue

  where

    execView = do
      phase <- getPhase
      when (phase == OpponentMove || phase == Watching) $
        requestGameState >> updatePhase
  
handleEvent appState _ = continue appState

isArrow :: V.Key -> Bool
isArrow k = case k of
  V.KLeft   -> True
  V.KRight  -> True
  V.KUp     -> True
  V.KDown   -> True
  _         -> False







