{-# LANGUAGE RecordWildCards #-}

module Main where

import           Relude                  hiding ( init )

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact

main :: IO ()
main = play (InWindow "Connect Four" (700, 600) (10, 10))
            black
            0
            init
            drawPicture
            handleEvent
            (const id)

init :: World
init = World { viewPort = ViewPort 600 700 }

newtype World = World
  { viewPort :: ViewPort
  } deriving Show

data ViewPort = ViewPort
  { viewPortLength :: Int
  , viewPortWidth :: Int
  } deriving Show

handleEvent :: Event -> World -> World
handleEvent (EventResize (w, l)) oldWorld =
  let newWorld = oldWorld { viewPort = ViewPort l w }
  in  traceShow newWorld newWorld
handleEvent _ v = v

drawPicture :: World -> Picture
drawPicture World {..} =
  let width'  = viewPortWidth viewPort
      length' = viewPortLength viewPort
      (boardWidth, boardLength) =
          if (toRational width' / toRational length') <= (7.0 / 6.0)
            then
              let boardWidth'  = fromIntegral width'
                  boardLength' = fromRational $ toRational width' * (6 / 7)
              in  (boardWidth', boardLength')
            else
              let boardWidth'  = fromRational $ toRational length' * (7 / 6)
                  boardLength' = fromIntegral length'
              in  (boardWidth', boardLength')
  in  color yellow $ rectangleSolid boardWidth boardLength

