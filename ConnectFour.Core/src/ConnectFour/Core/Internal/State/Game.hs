-- | All of the logic related to the game's state.
module ConnectFour.Core.Internal.State.Game
  ( Status
  , StatusView(..)
  , State
  , StateView(..)
  , Slot
  , view
  , init
  , undo
  , playMove
  
  , createHorizontalRange
  , createVerticalRange
  )
where

import           Relude                  hiding ( init
                                                , state
                                                , State
                                                )
import qualified ConnectFour.Core.Internal.State.Board
                                               as Board
import           Data.Array                     ( (!) )

type Slot = (Board.Column, Board.Row)

-- | The status of the game's state.
data Status
  = OnGoing_ Board.Chip
  | Tie_
  | Win_ Board.Chip

-- | A view of the game's status.
data StatusView
  = OnGoing Board.Chip
  | Tie
  | Win Board.Chip
  deriving Show

-- | The state of the game.
data State = State
  { _boardState :: Board.State
  , _gameStatus :: Status
  }

-- | A view of the game's state.
data StateView = StateView
  { boardState :: Board.Snapshot
  , gameStatus :: StatusView
  } deriving Show

-- | Get a view of the game's state
view :: State -> StateView
view (State board status) =
  let boardSnapshot = Board.snapshot board
      statusView    = case status of
        OnGoing_ chip -> OnGoing chip
        Tie_          -> Tie
        Win_ chip     -> Win chip
  in  StateView boardSnapshot statusView

-- | Initialize the state of the game.
init :: State
init = State Board.init (OnGoing_ Board.RedChip)

-- | Undo the last move.
undo :: State -> State
undo state =
  let undoState newChip = case Board.undo (_boardState state) of
        Nothing       -> state
        Just newBoard -> State newBoard (OnGoing_ newChip)
  in  case _gameStatus state of
        OnGoing_ chip -> undoState $ otherChip chip
        Win_     chip -> undoState chip
        Tie_          -> undoState Board.BlueChip

-- | Play a move if possible.
playMove :: Board.Column -> State -> State
playMove _ gameState@(State _ (Win_ _)) = gameState
playMove _ gameState@(State _ Tie_      ) = gameState
playMove column gameState@(State board (OnGoing_ chip)) =
  case Board.dropChip chip column board of
    Nothing       -> gameState
    Just newBoard -> case findWinner newBoard of
      Just winner -> State newBoard (Win_ winner)
      Nothing     -> if Board.isFull newBoard
        then State newBoard Tie_
        else State newBoard (OnGoing_ $ otherChip chip)

-- | Compute the opposite chip
otherChip :: Board.Chip -> Board.Chip
otherChip Board.RedChip  = Board.BlueChip
otherChip Board.BlueChip = Board.RedChip


-- | Find the winner.
findWinner :: Board.State -> Maybe Board.Chip
findWinner state = case Board.lastMove state of
  Nothing -> Nothing
  Just (column, row, chip) ->
    let boardSnapshot = Board.snapshot state
        slot          = (column, row)
        slotToChip (c, r) = (boardSnapshot ! c) ! r
        isTheSameChip c = Just chip == c 
        movesToCheck =
            slide 4 (slotToChip <$> createVerticalRange slot)
              <> slide 4 (slotToChip <$> createHorizontalRange slot)
              <> slide 4 (slotToChip <$> createLeftToRightDiagonalRange slot)
              <> slide 4 (slotToChip <$> createRightToLeftDiagonalRange slot)
    in  if any (all isTheSameChip) movesToCheck
           then Just chip
           else Nothing

slide :: Int -> [a] -> [[a]]
slide _ [] = []
slide n xs | n <= 0 || length xs < n = []
slide n xs@(_ : tail') = part : slide n tail' where part = take n xs

createVerticalRange :: Slot -> [Slot]
createVerticalRange slot =
  let (columnIndex, rowIndex) = toUnsafeIndex slot
      slotRange = [ (columnIndex, r) | r <- [rowIndex - 3 .. rowIndex + 3] ]
  in  toSafeRange slotRange

createHorizontalRange :: Slot -> [Slot]
createHorizontalRange slot =
  let (columnIndex, rowIndex) = toUnsafeIndex slot
      slotRange = [ (c, rowIndex) | c <- [columnIndex - 3 .. columnIndex + 3] ]
  in  toSafeRange slotRange

createLeftToRightDiagonalRange :: Slot -> [Slot]
createLeftToRightDiagonalRange slot =
  let (columnIndex, rowIndex) = toUnsafeIndex slot
      slotRange               = zip [columnIndex - 3 .. columnIndex + 3]
                                    [rowIndex - 3 .. rowIndex + 3]
  in  toSafeRange slotRange

createRightToLeftDiagonalRange :: Slot -> [Slot]
createRightToLeftDiagonalRange slot =
  let (columnIndex, rowIndex) = toUnsafeIndex slot
      slotRange = zip [columnIndex + 3, columnIndex + 2 .. columnIndex - 3]
                      [rowIndex - 3 .. rowIndex + 3]
  in  toSafeRange slotRange

toSafeRange :: [(Int, Int)] -> [Slot]
toSafeRange slotRange =
  [ (toEnum c, toEnum r) | i@(c, r) <- slotRange, isSafeIndex i ]

toUnsafeIndex :: Slot -> (Int, Int)
toUnsafeIndex (column, row) = (fromEnum column, fromEnum row)

isSafeIndex :: (Int, Int) -> Bool
isSafeIndex (column, row) =
  let maxColumnIndex = fromEnum (maxBound :: Board.Column)
      minColumnIndex = fromEnum (minBound :: Board.Column)
      maxRowIndex    = fromEnum (maxBound :: Board.Row)
      minRowIndex    = fromEnum (minBound :: Board.Row)
  in  column >= minColumnIndex
        && column <= maxColumnIndex
        && row >= minRowIndex
        && row <= maxRowIndex
