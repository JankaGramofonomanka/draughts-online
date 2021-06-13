module Errors where


-- make an alias in case we want more sophisticated errors in the future
type Error = String


outOfBoardError :: Error
outOfBoardError = "attempt to place out of board"

placedOnWhiteError :: Error
placedOnWhiteError = "attempt to place on a white field"

pieceNonExistentError :: Error
pieceNonExistentError = "piece does not exist"

piecesCollideError :: Error
piecesCollideError = "pices collide"

opponentPieceError :: Error
opponentPieceError = "attempt to acces piece of the opponent"

unknownDirError :: Error
unknownDirError = "unknown direction"

invalidInputError :: Error
invalidInputError = "invalid input"

cannotMoveError :: Error
cannotMoveError = "cannot move that piece"

fieldNotEmptyError :: Error
fieldNotEmptyError = "field is not empty"

opponentMoveError :: Error
opponentMoveError = "move of the opponent"

cannotJoinError :: Error
cannotJoinError = "both players alredy joined"

lockedDirectionError :: Error
lockedDirectionError = "cannot move in that direction"
