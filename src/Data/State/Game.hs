module Data.State.Game
  ( Player(..)
  , WinningSlots(..)
  , GameState
  , Data.State.Game.init
  , playTurn
  )
where

import           Prelude
import           Data.Monoid                    ( All(..) )
import           Data.Maybe                     ( isJust )
import qualified Data.State.Board              as Board
import           Data.State.Board               ( Board )

data Player
  = PlayerOne
  | PlayerTwo
  deriving (Show, Eq)

newtype WinningSlots = WinningSlots [(Board.Column, Board.Row)]
  deriving (Show, Eq)

data GameState
  = OnGoing Player Board
  | Win Player WinningSlots Board
  | Tie Board
  deriving (Show, Eq)

init :: GameState
init = OnGoing PlayerOne Board.init

playTurn :: Board.Column -> GameState -> GameState
playTurn column gameState@(OnGoing player board) =
  let chip = playerToChip player
  in  case Board.dropInto chip column board of
        Board.UpdateFailed              -> gameState
        Board.UpdateSuccessful newBoard -> case findAWin newBoard of
          Just (winningPlayer, winningSlots) ->
            Win winningPlayer winningSlots newBoard
          _ -> if isBoardFull newBoard
            then Tie newBoard
            else OnGoing (otherPlayer player) newBoard
playTurn _ gameState = gameState

isBoardFull :: Board -> Bool
isBoardFull board =
  let columns = flip Board.column board <$> [minBound .. maxBound]
  in  getAll . mconcat $ All . all isJust <$> columns

findAWin :: Board -> Maybe (Player, WinningSlots)
findAWin = _


playerToChip :: Player -> Board.Chip
playerToChip PlayerOne = Board.RedChip
playerToChip PlayerTwo = Board.BlueChip

otherPlayer :: Player -> Player
otherPlayer PlayerOne = PlayerTwo
otherPlayer PlayerTwo = PlayerOne
