{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DataFormatting where


import Data.Foldable
import Data.String (fromString)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Vector hiding (foldl')
import Data.Aeson

import GameState




instance ToJSON Color where
  toJSON White = String "white"
  toJSON Black = String "black"

    



instance ToJSON GameState where
  
  toJSON GameState  { board = board,
                      dimension = (w, h),
                      lock = lock,
                      mover = mover
                    } 

    = Object $ HM.fromList  [ ("board",   fromBoard board),
                              ("width",   toJSON w),
                              ("height",  toJSON h),
                              ("lock",    toJSON lock),
                              ("mover",   toJSON mover)
                            ]
  
    where
      
      fromBoard :: Board -> Value
      fromBoard board = Array $ jsonArr where
        jsonArr = foldl' foldFunc empty $ M.toList board

        foldFunc arr el = snoc arr $ mkJSON el
        mkJSON ((x, y), color)
          = Object $ HM.fromList  [ ("x",     toJSON x),
                                    ("y",     toJSON y),
                                    ("color", toJSON color)
                                  ]

  
  


instance ToJSON Direction where
  toJSON TopLeft  = String "TL"
  toJSON TopRight = String "TR"
  toJSON BotLeft  = String "BL"
  toJSON BotRight = String "BR"

