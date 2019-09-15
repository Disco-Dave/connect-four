-- | High level API for interacting with the game.
module ConnectFour.Core
  ( Command(..)
  , init
  , update
  )
where

import           ConnectFour.Core.Internal.State.Board ( Column )
import qualified ConnectFour.Core.Internal.State.Game as Game

-- | Initialize a new game.
init :: Game.State
init = Game.init

-- | All valid commands you may send to the game.
data Command
  = Restart
  | Undo
  | PlayMove Column

-- | Update the game via a command.
update :: Command -> Game.State -> Game.State
update Restart           _         = init
update Undo              gameState = Game.undo gameState
update (PlayMove column) gameState = Game.playMove column gameState
