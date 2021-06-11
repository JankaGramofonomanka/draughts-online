{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Widgets where


import qualified Data.Map as M

import Brick hiding (Direction)
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty as V


import GameState
import AppState





myWidget :: Widget ()
myWidget = withBorderStyle unicode 
  $ borderWithLabel (str "rectangle")
  $ hCenter
  $ str "RECTANGLE"


fieldWidget :: Board -> Int -> Int -> Widget ()
--fieldWidget board y x = (wgt <+> vBorder) <=> hBorder
fieldWidget board y x = case M.lookup (x, y) board of

  Just Black  -> withAttr blackPieceAttr $ str " ○ "
  Just White  -> withAttr whitePieceAttr $ str " ● "
  Nothing     -> withAttr fieldAttr $ str "   "

  where
    fieldAttr = if validPos (x, y) then blackPosAttr else whitePosAttr




drawBoard :: AppState -> Widget ()
drawBoard appState = let
    gameSt = gameState appState
    (w, h) = dimension gameSt
    brd = board gameSt

    drawRow y = hBox $ map (fieldWidget brd y) [0..w-1]
    drawBrd = vBox $ map drawRow [0..h-1]
    
  
  in withBorderStyle unicode 
    $ borderWithLabel (str "BOARD")
    $ hCenter
    $ joinBorders
    $ withBorderStyle unicodeBold
    $ drawBrd
      



dirWidget :: AppState -> Direction -> Widget ()
dirWidget appState dir = withBorderStyle unicode
  $ borderWithLabel (str label)
  $ hCenter
  $ str $ show dir

  where    
    label = if selectedDir appState == dir then "THIS" else ""


drawDirs :: AppState -> Widget ()
drawDirs appState = 
      (dirWidget appState TopLeft <+> dirWidget appState TopRight)
  <=> (dirWidget appState BotLeft <+> dirWidget appState BotRight)


drawApp :: AppState -> [Widget ()]
drawApp appState = case phase appState of
  PieceSelection  -> return $ drawBoard appState
  MoveSelection   -> return $ drawBoard appState <=> drawDirs appState
  Waiting         -> return $ drawBoard appState










blackPosAttr, whitePosAttr, blackPieceAttr, whitePieceAttr :: AttrName
blackPosAttr = "blackPosAttr"
whitePosAttr = "whitePosAttr"
blackPieceAttr = "plackPieceAttr"
whitePieceAttr = "whitePiece"




theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blackPosAttr, V.black `on` V.black)
  , (whitePosAttr, V.white  `on` V.white)
  , (blackPieceAttr, V.white `on` V.black)
  , (whitePieceAttr, V.white `on` V.black)
  ]

