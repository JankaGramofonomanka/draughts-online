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






