-- | All of the logic related to the game's state.
module ConnectFour.Core.Internal.State.Game
  ( Status
  , StatusView(..)
  , State
  , StateView(..)
  , view
  , init
  , undo
  , playMove
  )
where

import           Relude                  hiding ( init
                                                , state
                                                , State
                                                )
import qualified ConnectFour.Core.Internal.State.Board
                                               as Board

-- | The status of the game's state.
data Status
  = OnGoing_ Board.Chip
  | Tie_
  | Win_ Board.Chip (Set (Board.Column, Board.Row))

-- | A view of the game's status.
data StatusView
  = OnGoing Board.Chip
  | Tie
  | Win Board.Chip (Set (Board.Column, Board.Row))
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
        OnGoing_ chip          -> OnGoing chip
        Tie_                   -> Tie
        Win_ chip winningSlots -> Win chip winningSlots
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
        Win_ chip _   -> undoState chip
        Tie_          -> undoState Board.BlueChip

-- | Play a move if possible.
playMove :: Board.Column -> State -> State
playMove _ gameState@(State _ (Win_ _ _)) = gameState
playMove _ gameState@(State _ Tie_      ) = gameState
playMove column gameState@(State board (OnGoing_ chip)) =
  case Board.dropChip chip column board of
    Nothing       -> gameState
    Just newBoard -> case findWinner newBoard of
      Just (winner, winningMoves) -> State newBoard (Win_ winner winningMoves)
      Nothing                     -> if Board.isFull newBoard
        then State newBoard Tie_
        else State newBoard (OnGoing_ $ otherChip chip)

-- | Find the winner.
findWinner :: Board.State -> Maybe (Board.Chip, Set (Board.Column, Board.Row))
findWinner state =
  let lastMove      = Board.lastMove state
      boardSnapshot = Board.snapshot state
  in  traceShow lastMove Nothing -- What is the most efficient and elegant way to do this?

-- | Compute the opposite chip
otherChip :: Board.Chip -> Board.Chip
otherChip Board.RedChip  = Board.BlueChip
otherChip Board.BlueChip = Board.RedChip
