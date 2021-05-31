{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module SimpleInterface where

import qualified Data.Map as M
import Text.Read (readMaybe)
import Data.Maybe

import Control.Monad.Except
import Control.Monad.State.Strict

import GameState
import Errors ( Error,
                unknownDirError,
                invalidInputError
              )


readDir :: MonadError Error m => String -> m Direction
readDir s = case s of
  "TL" -> return TopLeft
  "TR" -> return TopRight
  "BL" -> return BotLeft
  "BR" -> return BotRight
  _ -> throwError unknownDirError

readDirection :: (MonadError Error m, MonadIO m) => m Direction
readDirection = do
  liftIO $ putStrLn "chose direction"
  l <- liftIO getLine 
  readDir l

readInt :: (MonadError Error m, MonadIO m) => m Int
readInt = do
  l <- liftIO getLine
  let maybeX = readMaybe l
  when (maybeX == Nothing) $ throwError invalidInputError
  return $ fromJust maybeX

readPos :: (MonadError Error m, MonadIO m) => m Pos
readPos = do
  liftIO $ putStrLn "chose x"
  x <- readInt

  liftIO $ putStrLn "chose y"
  y <- readInt

  return  (x, y)


printBoard :: GameState -> String
printBoard (GameState { board = board, dimension = (w, h), .. }) = do
  y <- [-1..h-1]
  x <- [-1..w]

  if x == w then return '\n' else do  
  if x == -1 && y == -1 then return ' ' else do
  if x == -1 then return $ head $ show y else do
  if y == -1 then return $ head $ show x else do

  let maybePiece = M.lookup (x, y) board

  case maybePiece of
    Nothing     -> if x `mod` 2 == y `mod` 2 then return ' ' else return '_'
    Just Black  -> return '*'
    Just White  -> return 'o'


makeMove :: (MonadState GameState m, MonadError Error m, MonadIO m) => 
  Color -> m Color 
makeMove color = do
  state <- get
  liftIO $ putStrLn $ printBoard state

  pos <- readPos
  dir <- readDirection

  movePiece color pos dir



runGame :: (MonadState GameState m, MonadError Error m, MonadIO m) =>
  Color -> m Color
runGame color = do

  let mkMov = makeMove color
  nextPlayer <- catchError mkMov (\e -> printError e >> return color)

  numBlacks <- getNumPieces Black
  numWhites <- getNumPieces White

  if numBlacks == 0 then
    return White 
  else if numWhites == 0 then
    return Black
  else do
    runGame nextPlayer

  where
    printError error = liftIO $ putStrLn $ "Error: " ++ error


type Game = StateT GameState (ExceptT Error IO) Color
reallyRunGame :: IO ()
reallyRunGame = do 
  result <- evalStateT (runExceptT $ runGame White) (initState 8 8 3)


  case result of
    Left e        -> putStrLn $ "Error: " ++ e
    Right winner  -> putStrLn $ "The winner is: " ++ (show winner)



