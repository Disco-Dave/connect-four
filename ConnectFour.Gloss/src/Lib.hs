module Lib
  ( Lib.init
  , handleEvent
  , drawPicture
  )
where

import           Relude

import           Data.Ratio
import           Data.Array                     ( Array
                                                , (!)
                                                )

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact

import qualified ConnectFour.Core              as Core





{- MODEL -}

data World = World
  { scaleFactor :: Float
  , gameState :: Core.State
  }

init :: World
init = World { scaleFactor = 1.0, gameState = Core.init }





{- UPDATE -}

handleEvent :: Event -> World -> World
handleEvent (EventResize (w, l)) oldWorld = if (w % l) <= (700 % 660)
  then oldWorld { scaleFactor = fromIntegral w / 700.0 }
  else oldWorld { scaleFactor = fromIntegral l / 660.0 }

handleEvent evt@(EventKey (MouseButton LeftButton) Up _ (x, y)) oldWorld =
  let World {..} = oldWorld
  in  if x < (-350 * scaleFactor) || x > (350 * scaleFactor)
        then oldWorld
        else if
          | y <= (330 * scaleFactor) && y > (270 * scaleFactor) -> 
            handleCommandEvent x oldWorld
          | y <= (270 * scaleFactor) && y > (-330 * scaleFactor) -> 
            handleBoardEvent x oldWorld
          | otherwise -> 
            oldWorld

handleEvent _ v = v

handleCommandEvent :: Float -> World -> World
handleCommandEvent x oldWorld = 
  let World {..} = oldWorld
      newGameState = 
        if
          | x >= (scaleFactor * 160) && x < (scaleFactor * 260) ->
            Core.update Core.Restart gameState
          | x >= (scaleFactor * 260) && x < (scaleFactor * 350) ->
            Core.update Core.Undo gameState
          | otherwise ->
            gameState
   in oldWorld { gameState = newGameState }

handleBoardEvent :: Float -> World -> World
handleBoardEvent x oldWorld =
  let World {..}   = oldWorld
      newGameState = if
        | x <= (-250 * scaleFactor) -> 
          Core.update (Core.PlayMove Core.ColumnOne) gameState
        | x <= (-150 * scaleFactor) -> 
          Core.update (Core.PlayMove Core.ColumnTwo) gameState
        | x <= (-50 * scaleFactor) -> 
          Core.update (Core.PlayMove Core.ColumnThree) gameState
        | x <= (50 * scaleFactor) -> 
          Core.update (Core.PlayMove Core.ColumnFour) gameState
        | x <= (150 * scaleFactor) -> 
          Core.update (Core.PlayMove Core.ColumnFive) gameState
        | x <= (250 * scaleFactor) -> 
          Core.update (Core.PlayMove Core.ColumnSix) gameState
        | otherwise -> 
          Core.update (Core.PlayMove Core.ColumnSeven) gameState
  in  oldWorld { gameState = newGameState }





{- VIEW -}

drawPicture :: World -> Picture
drawPicture world@World {..} = scale scaleFactor scaleFactor
  $ Pictures [drawBoard world, drawCommandBar world]

drawBoard :: World -> Picture
drawBoard World {..} =
  let (Core.view -> Core.StateView{..}) = gameState
      drawColumn' x column = drawColumn x (boardState ! column)
      xs = [-300, -200, -100, 0, 100, 200, 300]
  in  translate 0 (-30)
        $ Pictures
        $ color yellow (rectangleSolid 700 600)
        : [ drawColumn' x column
          | (x, column) <- zip xs [minBound .. maxBound]
          ]

drawColumn :: Float -> Array Core.Row (Maybe Core.Chip) -> Picture
drawColumn x column =
  let ys = [250, 150, 50, -50, -150, -250]
  in  Pictures
        [ translate x y $ drawChip (column ! row)
        | (y, row) <- zip ys [minBound .. maxBound]
        ]

drawChip :: Maybe Core.Chip -> Picture
drawChip Nothing              = color black $ circleSolid 45
drawChip (Just Core.RedChip ) = color red $ circleSolid 45
drawChip (Just Core.BlueChip) = color blue $ circleSolid 45

drawCommandBar :: World -> Picture
drawCommandBar World {..} =
  let (Core.view -> Core.StateView{..}) = gameState
      chipToText Core.RedChip  = "Red"
      chipToText Core.BlueChip = "Blue"
      statusText = case gameStatus of
        Core.OnGoing chip -> "Turn: " <> chipToText chip
        Core.Win chip _   -> "Winner: " <> chipToText chip
        Core.Tie          -> "Tie"
  in  translate 0 300 $ Pictures
        [ color black $ rectangleSolid 700 60
        , translate 160 (-15) $ color white $ scale 0.25 0.25 $ text "Reset"
        , translate 260 (-15) $ color white $ scale 0.25 0.25 $ text "Undo"
        , translate (-340) (-15) $ color white $ scale 0.25 0.25 $ text
          statusText
        ]
