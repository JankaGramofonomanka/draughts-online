module Main where


import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty as V


import Widgets
import AppState







app :: App AppState () ()
app = App { appDraw = drawApp
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  endState <- defaultMain app initAppState
  print $ phase endState
  
