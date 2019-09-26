{-# LANGUAGE TypeApplications #-}

-- | Holds all of the logic for creating and manipulating
-- the state of a connect-four board.
module ConnectFour.Core.Internal.State.Board
  ( Row(..)
  , Column(..)
  , Chip(..)
  , State
  , init
  , dropChip
  , undo
  , isFull
  , snapshot
  , Snapshot
  , lastMove
  )
where

import           Relude                  hiding ( State
                                                , init
                                                )
import           GHC.Arr                        ( unsafeIndex )
import qualified Data.Array                    as Array
import           Data.Array                     ( Array
                                                , Ix(..)
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

instance Ix Row where
  range (s, e) = [s .. e]
  inRange (s, e) i = i `elem` range (s, e)
  unsafeIndex (s, _) i = fromEnum i - fromEnum s

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

instance Ix Column where
  range (s, e) = [s .. e]
  inRange (s, e) i = i `elem` range (s, e)
  unsafeIndex (s, _) i = fromEnum i - fromEnum s

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
dropChip chip column board@(State moves) = if isColumnFull column board
  then Nothing
  else Just $ State $ (chip, column) : moves

-- | Undo the last move played.
undo :: State -> Maybe State
undo = fmap State . viaNonEmpty tail . _moves

-- | Checks if a column is full on the board.
isColumnFull :: Column -> State -> Bool
isColumnFull column (State moves) =
  let columnHeight    = length $ filter ((== column) . snd) moves
      maxColumnHeight = 1 + fromEnum (maxBound :: Row)
  in  columnHeight >= maxColumnHeight

isFull :: State -> Bool
isFull (State moves) = moveLength >= maxLength
 where
  moveLength = length moves
  maxLength =
    (fromEnum (maxBound :: Row) + 1) * (fromEnum (maxBound :: Column) + 1)

-- | A snapshot of the board's state.
type Snapshot = Array Column (Array Row (Maybe Chip))

-- | Create a snapshot of the board's state.
snapshot :: State -> Snapshot
snapshot (State moves) = Array.array (minBound, maxBound) columns
 where
  leftZip []        _         = []
  leftZip (ah : at) []        = (ah, Nothing) : leftZip at []
  leftZip (ah : at) (bh : bt) = (ah, Just bh) : leftZip at bt

  columns = [ (column, getColumnSlots column) | column <- [minBound ..] ]

  getColumnSlots column = Array.array (minBound, maxBound) slotsWithRow
   where
    slots = reverse [ chip | (chip, column') <- moves, column' == column ]
    slotsWithRow = leftZip [maxBound, pred maxBound ..] slots

lastMove :: State -> Maybe (Column, Row, Chip)
lastMove (State []) = Nothing
lastMove (State ((chip, column) : xs)) =
  let columnHeight    = length $ filter ((== column) . snd) xs
      maxColumnHeight = fromEnum $ maxBound @Row
      row             = toEnum $ maxColumnHeight - columnHeight
  in  Just (column, row, chip)

