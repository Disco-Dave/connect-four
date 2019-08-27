-- | All of the logic related to the board's state.
module Data.State.Board
  ( Column(..)
  , Chip(..)
  , UpdateResult(..)
  , Board
  , Data.State.Board.init
  , dropInto
  )
where

import           Prelude
import           Control.Monad                  ( guard )
import           Data.State.Common
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

maxColumnHeight :: Int
maxColumnHeight = 7

dropIntoColumn :: Chip -> ColumnState -> UpdateResult ColumnState
dropIntoColumn chip (ColumnState oldState) = do
  guard $ length oldState < maxColumnHeight
  return $ ColumnState $ chip : oldState




-- Board stuff

-- | The state of the board.
newtype Board = Board { unwrapBoard :: Map Column ColumnState }
  deriving Show

-- | The initial board state.
init :: Board
init = Board $ fromList $ (, initColumnState) <$> enumFrom minBound

-- | Drop a chip into a column
dropInto :: Chip -> Column -> Board -> UpdateResult Board
dropInto chip column (Board oldState) = do
  newColumnState <- dropIntoColumn chip (oldState ! column)
  let newBoardState = update (const $ Just newColumnState) column oldState
  return $ Board newBoardState
