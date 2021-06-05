{-# LANGUAGE FlexibleContexts #-}


module Host where

import Data.Text
import Text.Read (readMaybe)
import Data.Maybe
import Data.String (fromString)

import Network.HTTP.Types
import Web.Spock hiding (get, put)

import Control.Monad.State
import Control.Monad.Except


import GameState
import Errors
import DataFormatting


-- utils ----------------------------------------------------------------------
getParam :: (MonadIO m, Read a) => String -> a -> ActionT m a
getParam key defaultVal = do
  mValue <- param $ fromString key
  case mValue of
    Nothing -> return defaultVal
    Just txt -> let
        mResult = readMaybe $ unpack txt
      in return $ fromMaybe defaultVal mResult



throwGameError :: (MonadError Error m, MonadIO m) => Error -> ActionT m a
throwGameError error = do
  setStatus status400
  text $ fromString error

catchGameError :: (MonadError Error m, MonadIO m) => 
  m a -> (Error -> ActionT m a) -> ActionT m a
catchGameError action handler = do
  result <- lift $ catchError (Right <$> action) (return . Left)

  case result of
    Left e -> handler e
    Right v -> return v


execGameAction :: (MonadError Error m, MonadIO m) => m a -> ActionT m a
execGameAction action = catchGameError action throwGameError




-- views ----------------------------------------------------------------------
stateView :: (MonadState GameState m, MonadIO m) => ActionT m ()
stateView = do
  st <- lift get
  json st

