module Main where

import           Relude                  hiding ( init )

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact

import           Data.Ratio

main :: IO ()
main = play (InWindow "Connect Four" (700, 660) (10, 10))
            black
            0
            init
            drawPicture
            handleEvent
            (const id)


{- MODEL -}

newtype World = World { scaleFactor :: Float }

init :: World
init = World 1.0



{- UPDATE -}

handleEvent :: Event -> World -> World
handleEvent (EventResize (w, l)) oldWorld = if (w % l) <= (35 % 33)
  then oldWorld { scaleFactor = fromIntegral w / 700.0 }
  else oldWorld { scaleFactor = fromIntegral l / 660.0 }

handleEvent _ v = v



{- VIEW -}

drawPicture :: World -> Picture
drawPicture world@World {..} =
  scale scaleFactor scaleFactor $ Pictures [drawBoard world, drawCommandBar world]

drawBoard :: World -> Picture
drawBoard World {..} =
  translate 0 (-30) $ color yellow $ rectangleSolid 700 600

drawCommandBar :: World -> Picture
drawCommandBar World {..} = translate 0 300 $ color red $ rectangleSolid 700 60
