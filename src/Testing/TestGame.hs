module Testing.TestGame where


import Data.Maybe
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State.Strict
import Test.QuickCheck

import GameState
import DataFormatting

import Testing.ArbitraryInstances


fromRight :: Either a b -> b
fromRight (Right x) = x


successful :: Either a b -> Bool
successful (Left _) = False
successful (Right _) = True


-- initial state valid --------------------------------------------------------
propInitStateValid :: Bool
propInitStateValid = successful result where

    result = runExcept (evalStateT validateState defaultInitState)


-- move while opponent is moving ---------------------------------------------- 
propOpponentMove :: Pos -> Direction -> GameState -> Bool
propOpponentMove pos dir gameSt = not $ successful result where

    color = mover gameSt
    result = runExcept (evalStateT (movePiece (opposite color) pos dir) gameSt)


-- move out of board ----------------------------------------------------------
propEdgeMoveD :: Int -> GameState -> Property
propEdgeMoveD x gameSt = let

    (w, h) = dimension gameSt
    
  in propEdgeMove (Left x) (h - 1) w gameSt BotLeft BotRight 

propEdgeMoveU :: Int -> GameState -> Property
propEdgeMoveU x gameSt = let

    (w, h) = dimension gameSt

  in propEdgeMove (Left x) 0 w gameSt TopLeft TopRight 

propEdgeMoveL :: Int -> GameState -> Property
propEdgeMoveL y gameSt = let

    (w, h) = dimension gameSt

  in propEdgeMove (Right y) 0 h gameSt TopLeft BotLeft

propEdgeMoveR :: Int -> GameState -> Property
propEdgeMoveR y gameSt = let

    (w, h) = dimension gameSt

  in propEdgeMove (Right y) (w - 1) h gameSt TopRight BotRight

-- this is not to be tested, only to create other tests
propEdgeMove ::
      Either Int Int
  ->  Int
  ->  Int
  ->  GameState
  ->  Direction
  ->  Direction
  ->  Property

propEdgeMove xory otherCoord bound gameSt dir1 dir2 = let
    (thisCoord, pos) = case xory of
      Left x -> (x, (x, otherCoord))
      Right y -> (y, (otherCoord, y))
    
    color = mover gameSt
    result1 = runExcept (evalStateT (movePiece color pos dir1) gameSt)
    result2 = runExcept (evalStateT (movePiece color pos dir2) gameSt)

    condition = (0 <= thisCoord && thisCoord < bound)

  in condition ==> not (successful result1) && not (successful result2)


-- move preserves validity (color) of position --------------------------------
propMovePreservesValidPos :: Pos -> Direction -> GameState -> Property
propMovePreservesValidPos p dir gameSt = let

    -- eliminate cases where `pos` is not the locked position
    pos = fromMaybe p $ lock gameSt

    color = mover gameSt

    -- force existence of the piece to avoid tons of discarded test cases
    action = modifyBoard (M.insert pos color) >> movePiece color pos dir
    result = runExcept (evalStateT action gameSt)

  in validPos pos ==> successful result ==> validPos pos


-- combo testing --------------------------------------------------------------
scoredNState :: Pos -> Color -> Direction -> GameState -> (Bool, GameState)
scoredNState pos color dir gameSt = let

    oppColor = opposite color
    numOppPiecesBefore = evalState (getNumPieces oppColor) gameSt

    action = movePiece color pos dir
    result = runExcept (execStateT action gameSt)

    gameStAfter = fromRight result
    numOppPiecesAfter = evalState (getNumPieces oppColor) gameStAfter

  in (successful result && numOppPiecesAfter < numOppPiecesBefore, gameStAfter)


propScorePossible :: Pos -> Color -> GameState -> Bool
propScorePossible pos color gameSt = let
    action = dirsWhereScorePossible color pos
    dirs = evalState action gameSt
  
  in not $ null dirs




propIsCombo :: Pos -> Direction -> GameState -> Property
propIsCombo = getPropCombo True

propNoCombo :: Pos -> Direction -> GameState -> Property
propNoCombo = getPropCombo False

getPropCombo :: Bool -> Pos -> Direction -> GameState -> Property
getPropCombo checkSuccess p dir gameSt = let

    -- eliminate cases where `pos` is not the locked position
    pos = fromMaybe p $ lock gameSt

    color = mover gameSt
    move = toMove dir
    nPos = move pos
    nnPos = move $ move pos

    allMoves = [ toMove d | d <- allDirections ]
    toClear = [ mv $ mv nnPos | mv <- allMoves ]

    -- force the game board to fit the test
    insertAction = mapM_ (modifyBoard . M.delete) toClear
                >> modifyBoard (M.insert pos color)
                >> modifyBoard (M.insert nPos $ opposite color)
                >> modifyBoard (M.delete nnPos)

    gameStOK = execState insertAction gameSt

    (scored, gameStAfter) = scoredNState pos color dir gameStOK
    
    canScore = propScorePossible nnPos color gameStAfter

    colorAfter = mover gameStAfter
    lockAfter = lock gameStAfter

  in if checkSuccess then
      scored ==> canScore ==> colorAfter == color && lockAfter == Just nnPos
    else
      scored ==> not canScore ==> colorAfter == opposite color





main :: IO ()
main = putStrLn "\nTest Game"
    >> quickCheck propInitStateValid
    >> quickCheck propOpponentMove
    >> quickCheck propEdgeMoveU
    >> quickCheck propEdgeMoveD
    >> quickCheck propEdgeMoveL
    >> quickCheck propEdgeMoveR
    >> quickCheck propMovePreservesValidPos
    >> quickCheck propIsCombo
    >> quickCheck propNoCombo


