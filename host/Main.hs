{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}


module Main where


import Data.IORef
import Data.String (fromString)

import Web.Spock.Core

import qualified Control.Monad.State as S
import Control.Monad.Except


import GameState
import Errors
import Host





runStateApp :: IORef s -> S.StateT s (ExceptT Error IO) a -> IO a
runStateApp r act = do
    s <- readIORef r
    result <- runExceptT (S.runStateT act s)
    
    case result of
      Right (x, s') -> atomicModifyIORef' r $ const (s', x)
      Left e -> throwError $ userError e
      


main :: IO ()
main = do
  r <- newIORef $ initState 8 8 3

  let runner = runStateApp r
  runSpock 11350 $ spockT runner $ do
    get root viewState
    post root storeState
    post (fromString "err") checkPos

    --get (fromString "state") stateView
    --put (fromString "move") moveView
    --post (fromString "join") joinView


viewState :: (S.MonadState GameState m, MonadIO m) => ActionT m ()
viewState = do
  (w, h) <- lift getDimension 
  text $ fromString $ "width: " ++ (show w) ++ ", hwight: " ++ (show h)

storeState :: (S.MonadState GameState m, MonadIO m) => ActionT m ()
storeState = do
  w <- getParam "w" 8
  h <- getParam "h" 8
  

  lift $ overWriteDimension (w, h)

  where
    overWriteDimension :: S.MonadState GameState m => (Int, Int) -> m ()
    overWriteDimension dim = do
      GameState { dimension = _, .. } <- S.get
      S.put $ GameState { dimension = dim, .. }

checkPos :: (S.MonadState GameState m, MonadIO m, MonadError Error m) =>
  ActionT m ()
checkPos = do
  x <- getParam "x" 0
  y <- getParam "y" 0

  msg <- execGameAction (assertOnBoard (x, y) >> return "OK")
  text $ fromString msg



