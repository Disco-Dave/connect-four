module Game
  ( GameState.init
  , Message(..)
  , update
  , glossUpdate
  , view
  )
where

import           Prelude
import           Data.Array                     ( (!) )
import           Data.State.Board               ( Row(..)
                                                , Column(..)
                                                , Chip(..)
                                                , Board
                                                )
import           Data.State.Game                ( GameState )
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import qualified Data.State.Game               as GameState
import qualified Data.State.Board              as Board

data Message
  = PlayTurn Board.Column
  | Reset

update :: Message -> GameState -> GameState
update message gameState = case message of
  PlayTurn column -> GameState.playTurn column gameState
  Reset           -> GameState.init

glossUpdate :: Event -> GameState -> GameState
glossUpdate event gameState = case event of
  EventKey (MouseButton LeftButton) Up _ (x, _)
    | x <= (-250) -> update (PlayTurn ColumnOne) gameState
    | x <= (-150) -> update (PlayTurn ColumnTwo) gameState
    | x <= (-50)  -> update (PlayTurn ColumnThree) gameState
    | x <= 50     -> update (PlayTurn ColumnFour) gameState
    | x <= 150    -> update (PlayTurn ColumnFive) gameState
    | x <= 250    -> update (PlayTurn ColumnSix) gameState
    | otherwise   -> update (PlayTurn ColumnSeven) gameState
  _ -> gameState


view :: GameState -> Picture
view gameState =
  let board = GameState.getBoard gameState
      rows =
          ($ board)
            <$> [ viewRow 250    RowOne
                , viewRow 150    RowTwo
                , viewRow 50     RowThree
                , viewRow (-50)  RowFour
                , viewRow (-150) RowFive
                , viewRow (-250) RowSix
                ]
  in  Pictures rows

viewRow :: Float -> Row -> Board -> Picture
viewRow y row board =
  let row'    = Board.row row board
      xValues = [-300, -200, -100, 0, 100, 200, 300] :: [Float]
      chips   = zip xValues ((row' !) <$> [minBound .. maxBound])
  in  Pictures $ (\(x, chip) -> Translate x y $ viewChip chip) <$> chips

viewChip :: Maybe Chip -> Picture
viewChip Nothing         = Color black $ circleSolid 45
viewChip (Just RedChip ) = Color red $ circleSolid 45
viewChip (Just BlueChip) = Color blue $ circleSolid 45

