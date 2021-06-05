{-# LANGUAGE FlexibleContexts #-}


module Host where

import Data.Text
import Text.Read (readMaybe)
import Data.Maybe
import Data.String (fromString)

import Web.Spock

import Control.Monad.Except


import GameState
import Errors


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











