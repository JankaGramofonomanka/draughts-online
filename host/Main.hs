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
  r <- newIORef $ defaultInitState

  let runner = runStateApp r
  runSpock 11350 $ spockT runner $ do
    post root testStorage
    post (fromString "err") testErrors

    get (fromString "state") stateView
    put (fromString "move") moveView
    post (fromString "join") joinView


testStorage :: (S.MonadState GameState m, MonadIO m) => ActionT m ()
testStorage = do
  w <- getParam "w" 8
  h <- getParam "h" 8
  

  lift $ overWriteDimension (w, h)

  where
    overWriteDimension :: S.MonadState GameState m => (Int, Int) -> m ()
    overWriteDimension dim = do
      GameState { dimension = _, .. } <- S.get
      S.put $ GameState { dimension = dim, .. }

testErrors :: (S.MonadState GameState m, MonadIO m, MonadError Error m) =>
  ActionT m ()
testErrors = do
  x <- getParam "x" 0
  y <- getParam "y" 0

  msg <- execGameAction (assertOnBoard (x, y) >> return "OK")
  text $ fromString msg



