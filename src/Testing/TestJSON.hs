module Testing.TestJSON where

import Test.QuickCheck hiding (Success)

import Data.Aeson.Types
import DataFormatting
import GameState

import Testing.ArbitraryInstances



propJSON :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
propJSON x = fromJSON (toJSON x) == Success x


propJSONColor :: Color -> Bool
propJSONColor = propJSON

propJSONGameState :: GameState -> Bool
propJSONGameState = propJSON

propJSONDir :: Direction -> Bool
propJSONDir = propJSON

propJSONMoveInfo :: MoveInfo -> Bool
propJSONMoveInfo = propJSON


main :: IO ()
main = putStrLn "\nTest JSON"
    >> quickCheck propJSONColor
    >> quickCheck propJSONGameState
    >> quickCheck propJSONDir
    >> quickCheck propJSONMoveInfo
