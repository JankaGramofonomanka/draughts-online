{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Widgets where


import qualified Data.Map as M

import Brick hiding (Direction)
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V


import GameState
import AppState





fieldWidget :: AppState -> Int -> Int -> Widget ()
fieldWidget appState y x = case M.lookup (x, y) brd of

  Just Black  -> withAttr blackPieceAttr  $ str [lChar, '○', rChar]
  Just White  -> withAttr whitePieceAttr  $ str [lChar, '●', rChar]
  Nothing     -> withAttr fieldAttr       $ str [lChar, ' ', rChar]

  where
    fieldAttr = if validPos (x, y) then blackPosAttr else whitePosAttr

    brd = board $ gameState appState
    selected = selectedPos appState
    ph = phase appState

    lChar = if (x, y) == selected && ph /= Watching then '[' else ' '
    rChar = if (x, y) == selected && ph /= Watching then ']' else ' '




drawBoard :: AppState -> Widget ()
drawBoard appState = let
    gameSt = gameState appState
    (w, h) = dimension gameSt    

    drawRow y = hBox $ map (fieldWidget appState y) [0..w-1]
    drawBrd = vBox $ map drawRow [0..h-1]
    
  
  in withBorderStyle unicode 
    $ borderWithLabel (str "BOARD")
    $ hCenter
    $ joinBorders
    $ withBorderStyle unicodeBold
    $ drawBrd
      



dirWidget :: AppState -> Direction -> Widget ()
dirWidget appState dir = withBorderStyle style
  $ border
  $ padLeftRight 1
  $ str $ prettyShow dir

  where
    style = if selectedDir appState == dir then unicodeBold else unicode
    prettyShow d = case d of
      TopLeft   -> "↖"
      TopRight  -> "↗"
      BotLeft   -> "↙"
      BotRight  -> "↘"


drawDirs :: AppState -> Widget ()
drawDirs appState = hCenter $ 
      (dirWidget appState TopLeft <+> dirWidget appState TopRight)
  <=> (dirWidget appState BotLeft <+> dirWidget appState BotRight)

drawPlayer :: AppState -> Widget ()
drawPlayer AppState { player = mbColor , .. } = str 
  $ "Your color is: " ++ case mbColor of
    Nothing -> "not selected"
    Just color -> show color

drawPhase :: AppState -> Widget ()
drawPhase appState = let
    ph = phase appState

  in case ph of
    PieceSelection  -> str "Select piece to move"
    MoveSelection   -> str "Select where to move the piece"
    OpponentMove    -> str "Waiting for opponent ..."
    Watching        -> str "Watching the game ..."
    _               -> str ""


drawMsg :: AppState -> Widget ()
drawMsg AppState { msg = mMsg, .. } = case mMsg of
  Nothing -> str " "
  Just msg -> str msg

drawInfo :: AppState -> Widget ()
drawInfo appState = border
    $ hCenter (drawPlayer appState)
  <=> hCenter (drawPhase appState )
  <=> hCenter (drawMsg appState)


drawButton :: Button -> Button -> Widget ()
drawButton selected butt = withBorderStyle style
  $ border
  $ hCenter
  $ str $ toText butt

  where
    style = if selected == butt then unicodeBold else unicode

    toText b = case b of
      Play  -> "Play Game"
      Watch -> "Watch Game"
      Exit  -> "Exit"

drawMenu :: AppState -> Widget ()
drawMenu appState @ AppState { menuButton = butt, .. } = 
      drawMsg appState
  <=> drawButton butt Play
  <=> drawButton butt Watch 
  <=> drawButton butt Exit



drawApp :: AppState -> [Widget ()]
drawApp appState = if phase appState == Menu then
    return $ drawMenu appState
  else return $ drawInfo appState 
    <=> case phase appState of
          PieceSelection  -> drawBoard appState
          MoveSelection   -> drawBoard appState <=> drawDirs appState
          _               -> drawBoard appState










blackPosAttr, whitePosAttr, blackPieceAttr, whitePieceAttr :: AttrName
blackPosAttr = "blackPosAttr"
whitePosAttr = "whitePosAttr"
blackPieceAttr = "plackPieceAttr"
whitePieceAttr = "whitePiece"




theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blackPosAttr, V.white `on` V.black)
  , (whitePosAttr, V.black  `on` V.white)
  , (blackPieceAttr, V.white `on` V.black)
  , (whitePieceAttr, V.white `on` V.black)
  ]

