-- | All of the logic related to the game's state.
module ConnectFour.Core.Internal.State.Game
  ( Status
  , State
  , init
  , undo
  , playMove
  )
where

import           Prelude                 hiding ( init
                                                , state
                                                , State
                                                )
import qualified ConnectFour.Core.Internal.State.Board as Board

-- | The status of the game's state.
data Status
  = OnGoing Board.Chip
  | Tie
  | Win Board.Chip (Set (Board.Column, Board.Row))

-- | The state of the game.
data State = State
  { boardState :: Board.State
  , gameStatus :: Status
  }

-- | Initialize the state of the game.
init :: State
init = State Board.init (OnGoing Board.RedChip)

-- | Undo the last move.
undo :: State -> State
undo state =
  let undoState newChip = case Board.undo (boardState state) of
        Nothing       -> state
        Just newBoard -> State newBoard (OnGoing newChip)
  in  case gameStatus state of
        OnGoing chip -> undoState $ otherChip chip
        Win chip _   -> undoState $ otherChip chip
        Tie          -> undoState Board.BlueChip

-- | Play a move if possible.
playMove :: Board.Column -> State -> State
playMove _ gameState@(State _ (Win _ _)) = gameState
playMove _ gameState@(State _ Tie      ) = gameState
playMove column gameState@(State board (OnGoing chip)) =
  case Board.dropChip chip column board of
    Nothing       -> gameState
    Just newBoard -> case findWinner newBoard of
      Just (winner, winningMoves) -> State newBoard (Win winner winningMoves)
      Nothing                     -> if Board.isFull newBoard
        then State newBoard Tie
        else State newBoard (OnGoing $ otherChip chip)

-- | Find the winner.
findWinner :: Board.State -> Maybe (Board.Chip, Set (Board.Column, Board.Row))
findWinner _ = Nothing -- What is the most efficient and elegant way to do this?

-- | Compute the opposite chip
otherChip :: Board.Chip -> Board.Chip
otherChip Board.RedChip  = Board.BlueChip
otherChip Board.BlueChip = Board.RedChip
