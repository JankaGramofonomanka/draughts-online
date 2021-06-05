{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}


module Main where

import Data.Text
import Text.Read (readMaybe)
import Data.Maybe
import Data.IORef
import Data.String (fromString)

import Web.Spock hiding (get, post, put, delete)
import Web.Spock.Core

import qualified Control.Monad.State as S
import Control.Monad.Except


import GameState
import Errors





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



getParam :: (MonadIO m, Read a) => String -> a -> ActionT m a
getParam key defaultVal = do
  mValue <- param $ fromString key
  case mValue of
    Nothing -> return defaultVal
    Just txt -> let
        mResult = readMaybe $ unpack txt
      in return $ fromMaybe defaultVal mResult



throwGameError :: (MonadError Error m, MonadIO m) => Error -> ActionT m a
throwGameError error = text $ fromString error

catchGameError :: (MonadError Error m, MonadIO m) => 
  m a -> (Error -> ActionT m a) -> ActionT m a
catchGameError action handler = do
  result <- lift $ catchError (Right <$> action) (return . Left)

  case result of
    Left e -> handler e
    Right v -> return v


execGameAction :: (MonadError Error m, MonadIO m) => m a -> ActionT m a
execGameAction action = catchGameError action throwGameError
