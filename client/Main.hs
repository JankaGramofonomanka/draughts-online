module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty as V

import qualified GameState as GS

type AppState = ()


myWidget :: Widget ()
myWidget = withBorderStyle unicode 
  $ borderWithLabel (str "rectangle")
  $ hCenter
  $ padAll 1
  $ str "RECTANGLE"

dirWidget :: GS.Direction -> Widget ()
dirWidget dir = withBorderStyle unicode
  $ borderWithLabel (str "")
  $ hCenter
  $ str $ show dir

drawDirs :: Widget ()
drawDirs = 
      (dirWidget GS.TopLeft <+> dirWidget GS.TopRight)
  <=> (dirWidget GS.BotLeft <+> dirWidget GS.BotRight)

drawApp :: AppState -> [Widget ()]
drawApp _ = return $
  drawDirs



handleEvent appState (VtyEvent (V.EvKey V.KEsc [])) = halt appState 
handleEvent appState _ = continue appState 

theMap :: AttrMap
theMap = attrMap V.defAttr []

app :: App AppState () ()
app = App { appDraw = drawApp
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  defaultMain app ()
