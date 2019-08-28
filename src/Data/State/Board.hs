-- | All of the logic related to the board's state.
module Data.State.Board
  ( Column(..)
  , Row(..)
  , Chip(..)
  , UpdateResult(..)
  , Board
  , Data.State.Board.init
  , dropInto
  , column
  , row
  , slot
  )
where

import           Prelude
import           Control.Monad                  ( guard )
import           GHC.Arr                        ( unsafeIndex )
import qualified Data.Array                    as Array
import           Data.Array                     ( Array )
import           Data.State.Common
import           Data.Map                       ( (!)
                                                , update
                                                )
-- | Indentifies a row on the board.
data Row
  = RowOne
  | RowTwo
  | RowThree
  | RowFour
  | RowFive
  | RowSix
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Array.Ix Row where
  range (s, e) = [s .. e]
  inRange (s, e) i = i `elem` Array.range (s, e)
  unsafeIndex (s, _) i = fromEnum i - fromEnum s


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

instance Array.Ix Column where
  range (s, e) = [s .. e]
  inRange (s, e) i = i `elem` Array.range (s, e)
  unsafeIndex (s, _) i = fromEnum i - fromEnum s

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

dropIntoColumn :: Chip -> ColumnState -> UpdateResult ColumnState
dropIntoColumn chip (ColumnState oldState) = do
  guard $ length oldState <= fromEnum (maxBound :: Row) + 1
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
dropInto chip column' (Board oldState) = do
  newColumnState <- dropIntoColumn chip (oldState ! column')
  let newBoardState = update (const $ Just newColumnState) column' oldState
  return $ Board newBoardState

-- | Get a column of the board
column :: Column -> Board -> Array Row (Maybe Chip)
column column' (Board boardState) =
  let ColumnState columnState = boardState ! column'
      numberOfEmptySpots = fromEnum (maxBound :: Row) + 1 - length columnState
      chipStack = replicate numberOfEmptySpots Nothing <> fmap Just columnState
  in  Array.array (minBound, maxBound) $ zip [minBound .. maxBound] chipStack

-- | Get the row of the board
row :: Row -> Board -> Array Column (Maybe Chip)
row row' board =
  let columns' =
          [ (c, column c board Array.! row') | c <- [minBound .. maxBound] ]
  in  Array.array (minBound, maxBound) columns'

-- | Get a slot of the board
slot :: Row -> Column -> Board -> Maybe Chip
slot row' column' board = column column' board Array.! row'
