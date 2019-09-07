module Game
  ( GameState.init
  , Message(..)
  , update
  )
where

import qualified Data.State.Game               as GameState
import qualified Data.State.Board              as Board

data Message
  = PlayTurn Board.Column
  | Reset

update :: Message -> GameState.GameState -> GameState.GameState
update message gameState = case message of
  PlayTurn column -> GameState.playTurn column gameState
  Reset           -> GameState.init
