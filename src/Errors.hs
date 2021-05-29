module Errors where


-- make an alias in case we want more sophisticated errors in the future
type Error = String


outOfBoardError :: Error
outOfBoardError = "piece is placed out of board"

colorMismatchError :: Error
colorMismatchError = "piece color does not match field color"

pieceNonExistentError :: Error
pieceNonExistentError = "piece does not exist"

piecesCollideError :: Error
piecesCollideError = "pices collide"







