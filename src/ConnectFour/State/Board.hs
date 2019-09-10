-- | Holds all of the logic for creating and manipulating
-- the state of a connect-four board.
module ConnectFour.State.Board
  ( Row(..)
  , Column(..)
  , Chip(..)
  , State
  , init
  , dropChip
  , undo
  )
where

import           Prelude                 hiding ( State
                                                , init
                                                )

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
newtype State = State {_moves :: [(Chip, Column)]}

-- | Initialize an empty connect-four board.
init :: State
init = State []

-- | Drop a chip into a column on the board.
-- This will return a new board if there was room for
-- the chip, otherwise it will return Nothing.
dropChip :: Chip -> Column -> State -> Maybe State
dropChip chip column board@(State moves) =
  if isColumnFull column board
    then Nothing
    else Just $ State $ (chip, column) : moves

-- | Undo the last move played. If there are no moves to
-- be undone because the board is empty, then you get
-- back an empty board.
undo :: State -> Maybe State
undo = fmap State . viaNonEmpty tail . _moves

-- | Checks if a column is full on the board.
isColumnFull :: Column -> State -> Bool
isColumnFull column (State moves) =
  let columnHeight    = length $ filter ((== column) . snd) moves
      maxColumnHeight = 1 + fromEnum (maxBound :: Row)
  in  columnHeight >= maxColumnHeight
