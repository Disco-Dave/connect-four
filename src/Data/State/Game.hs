module Data.State.Game
  ( Player(..)
  , GameState(..)
  , GameView(..)
  , UpdateResult(..)
  , Game
  , getBoardState
  , getGameState
  , getActivePlayer
  , Data.State.Game.init
  , view
  , play
  )
where

import           Prelude
import           Data.State.Common
import qualified Data.State.Board              as Board
import           Data.State.Board               ( Board )

data Player
  = PlayerOne
  | PlayerTwo
  deriving (Show, Eq)

data GameState
  = OnGoing
  | Tie
  | Winner Player
  deriving (Show, Eq)

data Game = Game
  { _boardState :: Board
  , _gameState :: GameState
  , _activePlayer :: Player
  } deriving (Show, Eq)

data GameView = GameView
  { boardState :: Board
  , gameState :: GameState
  , activePlayer :: Player
  } deriving (Show, Eq)

view :: Game -> GameView
view Game {..} = GameView { boardState   = _boardState
                          , gameState    = _gameState
                          , activePlayer = _activePlayer
                          }

getBoardState :: Game -> Board
getBoardState = _boardState

getGameState :: Game -> GameState
getGameState = _gameState

getActivePlayer :: Game -> Player
getActivePlayer = _activePlayer

init :: Game
init = Game { _boardState   = Board.init
            , _gameState    = OnGoing
            , _activePlayer = PlayerOne
            }

play :: Board.Column -> Board.Chip -> Game -> UpdateResult Game
play = _
