module Main where


import Brick
import Brick.BChan
import Graphics.Vty
import Control.Concurrent
import Control.Monad (forever)

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
  eventChan <- newBChan 10
  let buildVty = mkVty defaultConfig
  initVty <- buildVty

  _ <- heartbeat eventChan

  endState <- customMain initVty buildVty (Just eventChan) app initAppState
  return ()
  

seconds :: Int -> Int
seconds n = n * 1000000

heartbeat chan = forkIO $ forever $ do
  threadDelay $ seconds 1
  writeBChan chan ()

