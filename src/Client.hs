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

-- some utils -----------------------------------------------------------------
mkURL :: String -> String -> String
mkURL host endpoint = let
    cleanHost = removeSlashBack $ removeHttp host
    cleanEndpoint = removeSlashFront endpoint

  in "http://" ++ cleanHost ++ "/" ++ cleanEndpoint

  where

    removeHttp s = case s of
      'h':'t':'t':'p':'/':'/':ss      -> ss
      'h':'t':'t':'p':'s':'/':'/':ss  -> ss
      _                               -> s
      
    removeSlashBack s = case s of
      ""  -> s
      _   -> case last s of
        '/' -> init s
        _   -> s
    
    removeSlashFront s = case s of
      ""      -> s
      '/':ss  -> ss
      _       -> s




catchStateTIO :: Exception e => s -> StateT s IO a -> (e -> IO s) -> IO s
catchStateTIO s exec handler = catch (execStateT exec s) handler

httpHandler :: AppState -> HttpException -> IO AppState
httpHandler appState e = return $ execState (setErrMsg e) appState

setErrMsg :: MonadState AppState m => HttpException -> m ()
setErrMsg e = case e of
  
  HttpExceptionRequest _ (StatusCodeException _ txt) -> 
    setMsg $ "WRONG! " ++ toString txt
  
  InvalidUrlException _ _ -> setMsgE "invalid URL"

  HttpExceptionRequest _ content -> setMsgE $ case content of

    ResponseTimeout           -> "host does not answer (response timeout)"
    ConnectionTimeout         -> "cannot connect to host (connection timeout)"
    ConnectionFailure _       -> "cannot connect to host (connection failure)"
    InvalidDestinationHost _  -> "cannot connect to host (ivalid host)"

    
    -- other exceptions that I do not expect
    _ -> "some unexpected exception"


  where

    setMsgE s = setMsg $ "ERROR! " ++ s






-------------------------------------------------------------------------------
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

  host <- getHost
  let url = mkURL host "move"

  let rqBody = toJSON $ MV (fromJust color, pos, dir)

  resp <- liftIO $ Rq.put url rqBody

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

  host <- getHost
  let url = mkURL host "state"

  resp <- liftIO $ Rq.get url
  jsonResp <- liftIO $ Rq.asJSON resp
  let newGameSt = jsonResp ^. Rq.responseBody

  putGameState newGameSt
  checkEndGame


joinGame :: (MonadState AppState m, MonadIO m) => m ()
joinGame = do

  host <- getHost
  let url = mkURL host "join"

  -- something to put in the request body
  let rqBody = toJSON ()

  resp <- liftIO $ Rq.post url rqBody
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
          





handleEnter :: AppState -> EventM n1 (Next AppState)
handleEnter appState = case phase appState of
  PieceSelection  -> continue $ execState (putPhase MoveSelection) appState 
  MoveSelection   -> execMove appState
  Menu            -> execMenuButton appState
  
  _               -> continue appState 




execMove :: AppState -> EventM n (Next AppState)
execMove appState = liftIO (catchErr wrapMkMove) >>= continue
  
  where
    catchErr act = catchStateTIO appState act handler
    wrapMkMove = unsetMsg >> mkMove

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
    Watch -> suspendAndResume $ catchErr (promptForHost >> watch)
    Play  -> suspendAndResume $ catchErr (promptForHost >> play)

    where
      catchErr act = catchStateTIO appState act $ httpHandler appState
      watch = unsetMsg >> requestGameState >> updatePhase
      play = unsetMsg >> joinGame >> requestGameState >> updatePhase

      promptForHost :: (MonadState AppState m, MonadIO m) => m ()
      promptForHost = do
        liftIO $ putStrLn "Enter the game host address:"
        host <- liftIO getLine
        putHost host



handleArrow :: MonadState AppState m => V.Key -> m ()
handleArrow k = do
  ph <- getPhase

  case ph of
    Menu            -> selectButton k
    MoveSelection   -> selectDir k
    PieceSelection  -> selectPos k
    OpponentMove    -> selectPos k
    _               -> return ()


handleEsc :: AppState -> EventM n (Next AppState)
handleEsc appState = continue $ execState handleEscS appState where

  handleEscS = do
    ph <- getPhase
    case ph of
      MoveSelection   -> putPhase PieceSelection
      _               -> putPhase Menu

  
handleEvent :: AppState -> BrickEvent n e -> EventM n1 (Next AppState)
handleEvent appState (VtyEvent (V.EvKey k [])) = if isArrow k then
    continue $ execState (handleArrow k) appState
    
  else case k of
    V.KEsc      -> handleEsc appState
    V.KEnter    -> handleEnter appState 

    _           -> continue appState

handleEvent appState (AppEvent e) = 
  liftIO (catchErr view) >>= continue

  where

    catchErr act  = catchStateTIO appState act $ httpHandler appState

    view = do
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







