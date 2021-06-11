module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty as V





myWidget :: Widget ()
myWidget = withBorderStyle unicode 
  $ borderWithLabel (str "rectangle")
  $ hCenter
  $ padAll 1
  $ str "RECTANGLE"




handleEvent appState (VtyEvent (V.EvKey V.KEsc [])) = halt appState 
handleEvent appState _ = continue appState 

theMap :: AttrMap
theMap = attrMap V.defAttr []

app :: App () () ()
app = App { appDraw = const [myWidget]
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  defaultMain app ()
