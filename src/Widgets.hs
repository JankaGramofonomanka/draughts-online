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
  $ hCenter
  $ str $ show dir

  where    
    style = if selectedDir appState == dir then unicodeBold else unicode


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
  [ (blackPosAttr, V.white `on` V.black)
  , (whitePosAttr, V.black  `on` V.white)
  , (blackPieceAttr, V.white `on` V.black)
  , (whitePieceAttr, V.white `on` V.black)
  ]

