module Widgets where




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
  -- $ padAll 1
  $ str "RECTANGLE"


drawBoard :: AppState -> Widget ()
drawBoard appState = let
    gameSt = gameState appState
    (w, h) = dimension gameSt
  
  in foldl (<=>) myWidget $ replicate h myWidget



dirWidget :: Direction -> Widget ()
dirWidget dir = withBorderStyle unicode
  $ borderWithLabel (str "")
  $ hCenter
  $ str $ show dir

drawDirs :: Widget ()
drawDirs = 
      (dirWidget TopLeft <+> dirWidget TopRight)
  <=> (dirWidget BotLeft <+> dirWidget BotRight)

drawApp :: AppState -> [Widget ()]
drawApp appState = case phase appState of
  PieceSelection  -> return $ drawBoard appState
  MoveSelection   -> return $ drawBoard appState <=> drawDirs
  Waiting         -> return $ drawBoard appState






