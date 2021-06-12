{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Widgets where


import qualified Data.Map as M

import Brick hiding (Direction)
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty as V


import GameState
import AppState





fieldWidget :: Board -> Pos -> Int -> Int -> Widget ()
fieldWidget brd selected y x = case M.lookup (x, y) brd of

  Just Black  -> withAttr blackPieceAttr  $ str [lChar, '○', rChar]
  Just White  -> withAttr whitePieceAttr  $ str [lChar, '●', rChar]
  Nothing     -> withAttr fieldAttr       $ str [lChar, ' ', rChar]

  where
    fieldAttr = if validPos (x, y) then blackPosAttr else whitePosAttr

    lChar = if (x, y) == selected then '[' else ' '
    rChar = if (x, y) == selected then ']' else ' '




drawBoard :: AppState -> Widget ()
drawBoard appState = let
    gameSt = gameState appState
    (w, h) = dimension gameSt
    brd = board gameSt
    sel = selectedPos appState

    drawRow y = hBox $ map (fieldWidget brd sel y) [0..w-1]
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


drawMsg :: AppState -> Widget ()
drawMsg AppState { msg = mMsg, .. } = case mMsg of
  Nothing -> str " "
  Just msg -> str msg

drawInfo :: AppState -> Widget ()
drawInfo appState = border
    $ hCenter
    $ drawPlayer appState
  <=> drawPhase appState 
  <=> drawMsg appState



drawApp :: AppState -> [Widget ()]
drawApp appState = return $ 
      drawInfo appState 
  <=> case phase appState of
    PieceSelection  -> drawBoard appState
    MoveSelection   -> drawBoard appState <=> drawDirs appState
    OpponentMove    -> drawBoard appState










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

