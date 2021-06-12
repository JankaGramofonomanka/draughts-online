module Main where


import Brick


import Widgets (drawApp, theMap)
import AppState 
import Client (handleEvent)







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
  
