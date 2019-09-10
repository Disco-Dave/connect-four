-- | Holds all of the logic for creating and manipulating
-- the state of a connect-four board.
module ConnectFour.State.Board
  ( Row(..)
  , Column(..)
  , Chip(..)
  , Board
  , ConnectFour.State.Board.init
  , dropChip
  , undo
  )
where

import           Prelude

-- | Identifies a row on the board.
data Row
  = RowOne
  | RowTwo
  | RowThree
  | RowFour
  | RowFive
  | RowSix
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Identifies a column on the board.
data Column
  = ColumnOne
  | ColumnTwo
  | ColumnThree
  | ColumnFour
  | ColumnFive
  | ColumnSix
  | ColumnSeven
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Identifies a chip that can be dropped onto the board.
data Chip
  = RedChip
  | BlueChip
  deriving Show

-- | Holds the state of the board.
newtype Board = Board {_moves :: [(Chip, Column)]}

-- | Initialize an empty connect-four board.
init :: Board
init = Board []

-- | Drop a chip into a column on the board.
-- This will return a new board if there was room for
-- the chip, otherwise it will return Nothing.
dropChip :: Chip -> Column -> Board -> Maybe Board
dropChip chip column board@(Board moves) = 
  if isColumnFull column board
    then Nothing
    else Just $ Board $ (chip, column) : moves

-- | Undo the last move played. If there are no moves to
-- be undone because the board is empty, then you get
-- back an empty board.
undo :: Board -> Board
undo b@(Board []) = b
undo (Board (_ : t)) = Board t

-- | Checks if a column is full on the board.
isColumnFull :: Column -> Board -> Bool
isColumnFull column (Board moves) =
  let chipsInColumn = filter ((== column) . snd) moves
  in  length chipsInColumn >= 7
