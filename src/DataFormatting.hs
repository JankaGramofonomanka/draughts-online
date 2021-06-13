{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DataFormatting where


import Data.Foldable
import Control.Applicative
import Data.String (fromString)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Aeson.Types


import GameState



instance ToJSON Color where
  toJSON White = String "white"
  toJSON Black = String "black"

instance FromJSON Color where
  parseJSON (String "white") = return White
  parseJSON (String "black") = return Black
  
  parseJSON _ = empty
    

instance ToJSON GameState where
  
  toJSON GameState  { board = board,
                      dimension = (w, h),
                      lock = lock,
                      excludedDirections = excludedDirs,
                      mover = mover,
                      joined = joined
                    } 

    = Object $ HM.fromList  [ ("board",         fromBoard board),
                              ("width",         toJSON w),
                              ("height",        toJSON h),
                              ("lock",          toJSON lock),
                              ("excludedDirs",  toJSON excludedDirs),
                              ("mover",         toJSON mover),
                              ("joined",        toJSON joined)
                            ]
  
    where
      
      fromBoard :: Board -> Value
      fromBoard board = Array $ jsonArr where
        jsonArr = foldl' foldFunc V.empty $ M.toList board

        foldFunc arr el = V.snoc arr $ mkJSON el
        mkJSON ((x, y), color)
          = Object $ HM.fromList  [ ("x",     toJSON x),
                                    ("y",     toJSON y),
                                    ("color", toJSON color)
                                  ]

instance FromJSON GameState where
  parseJSON (Object obj) = do
    w <- obj .: "width"
    h <- obj .: "height"

    lock <- obj .: "lock"
    excludedDirs <- obj .: "excludedDirs"
    mover <- obj .: "mover"

    boardArr <- obj .: "board"
    board <- toBoard boardArr

    joined <- obj .: "joined"


    return GameState  { board = board,
                        dimension = (w, h),
                        lock = lock,
                        excludedDirections = excludedDirs,
                        mover = mover,
                        joined = joined
                      }

    where
      toBoard :: Array -> Parser Board
      toBoard arr = V.foldl' foldF (pure M.empty) arr

      foldF :: Parser Board -> Value -> Parser Board
      foldF mBoard (Object o) = do
        board <- mBoard

        x     <- o .: "x"
        y     <- o .: "y"
        color <- o .: "color"

        return $ M.insert (x, y) color board


  parseJSON _ = empty

  
  


instance ToJSON Direction where
  toJSON TopLeft  = String "TL"
  toJSON TopRight = String "TR"
  toJSON BotLeft  = String "BL"
  toJSON BotRight = String "BR"

instance FromJSON Direction where
  parseJSON (String "TL") = return TopLeft
  parseJSON (String "TR") = return TopRight
  parseJSON (String "BL") = return BotLeft
  parseJSON (String "BR") = return BotRight
  parseJSON _ = empty



newtype MoveInfo = MV (Color, Pos, Direction)

instance ToJSON MoveInfo where
  toJSON (MV (c, (x, y), dir)) 
    = Object $ HM.fromList  [ ("color", toJSON c), 
                              ("x", toJSON x),
                              ("y", toJSON y),
                              ("direction", toJSON dir)
                            ]

instance FromJSON MoveInfo where
  parseJSON (Object obj) = do
    color <- obj .: "color"
    dir <- obj .: "direction"
    x <- obj .: "x"
    y <- obj .: "y"

    return $ MV (color, (x, y), dir)

  parseJSON _ = empty



