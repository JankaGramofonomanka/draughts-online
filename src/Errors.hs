module Errors where


-- make an alias in case we want more sophisticated errors in the future
type Error = String


outOfBoardError :: Error
outOfBoardError = "piece is placed out of board"

placedOnWhiteError :: Error
placedOnWhiteError = "pice is placed on a white field"

pieceNonExistentError :: Error
pieceNonExistentError = "piece does not exist"

piecesCollideError :: Error
piecesCollideError = "pices collide"

opponentPieceError :: Error
opponentPieceError = "attempt to acces piece of the opponent"







