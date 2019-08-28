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
import           Data.State.Common
import           Data.Array                     ( (//)
                                                , (!)
                                                , array
                                                , range
                                                , listArray
                                                , Ix(..)
                                                , Array
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

instance Ix Row where
  range (s, e) = [s .. e]
  inRange (s, e) i = i `elem` range (s, e)
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

instance Ix Column where
  range (s, e) = [s .. e]
  inRange (s, e) i = i `elem` range (s, e)
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
  guard $ length oldState < fromEnum (maxBound :: Row) + 1
  return $ ColumnState $ chip : oldState




-- Board stuff

-- | The state of the board.
newtype Board = Board { unwrapBoard :: Array Column ColumnState }
  deriving Show

-- | The initial board state.
init :: Board
init = Board boardArray
 where
  emptyColumns = initColumnState <$ enumFrom (minBound :: Column)
  boardArray   = listArray (minBound, maxBound) emptyColumns


-- | Drop a chip into a column
dropInto :: Chip -> Column -> Board -> UpdateResult Board
dropInto chip column' (Board oldState) = do
  newColumnState <- dropIntoColumn chip (oldState ! column')
  let newBoardState = oldState // [(column', newColumnState)]
  return $ Board newBoardState

-- | Get a column of the board
column :: Column -> Board -> Array Row (Maybe Chip)
column column' (Board boardState) =
  let ColumnState columnState = boardState ! column'
      numberOfEmptySpots = fromEnum (maxBound :: Row) + 1 - length columnState
      chipStack = replicate numberOfEmptySpots Nothing <> fmap Just columnState
  in  array (minBound, maxBound) $ zip [minBound .. maxBound] chipStack

-- | Get the row of the board
row :: Row -> Board -> Array Column (Maybe Chip)
row row' board =
  let columns' = [ (c, column c board ! row') | c <- [minBound .. maxBound] ]
  in  array (minBound, maxBound) columns'

-- | Get a slot of the board
slot :: Row -> Column -> Board -> Maybe Chip
slot row' column' board = column column' board ! row'
