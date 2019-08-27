-- | All of the logic related to the board's state.
module Data.State.Board
  ( Column(..)
  , Chip(..)
  , Board
  , Data.State.Board.init
  , dropInto
  , BoardUpdateResult(..)
  )
where

import           Prelude
import           Data.Map                       ( (!)
                                                , update
                                                )

-- Column Stuff

-- | Identifies a column on the board.
data Column
  = ColumnOne
  | ColumnTwo
  | ColumnThree
  | ColumnFour
  | ColumnFive
  | ColumnSix
  | ColumnSeven
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Identifies a chip that can be placed in a column.
data Chip
  = BlueChip
  | RedChip
  deriving (Show, Eq)

-- | The state of a column.
newtype ColumnState = ColumnState { unwrapColumnState :: [Chip] }
  deriving Show

-- | The initial state of a column.
initColumnState :: ColumnState
initColumnState = ColumnState []

data ColumnUpdateResult
  = ColumnFailedToUpdate
  | ColumUpdatedSuccessfully ColumnState

dropIntoColumn :: Chip -> ColumnState -> ColumnUpdateResult
dropIntoColumn chip (ColumnState oldState) = if length oldState >= 7
  then ColumnFailedToUpdate
  else ColumUpdatedSuccessfully $ ColumnState $ chip : oldState




-- Board stuff

-- | The state of the board.
newtype Board = Board { unwrapBoard :: Map Column ColumnState }
  deriving Show

-- | The initial board state.
init :: Board
init = Board $ fromList $ (, initColumnState) <$> enumFrom minBound

data BoardUpdateResult
  = BoardFailedToUpdate
  | BoardUpdatedSuccessfully Board
  deriving Show

-- | Drop a chip into a column
dropInto :: Chip -> Column -> Board -> BoardUpdateResult
dropInto chip column (Board oldState) =
  case dropIntoColumn chip (oldState ! column) of
    ColumnFailedToUpdate -> BoardFailedToUpdate
    ColumUpdatedSuccessfully newColumnState ->
      let newBoardState = update (const $ Just newColumnState) column oldState
      in  BoardUpdatedSuccessfully $ Board newBoardState
