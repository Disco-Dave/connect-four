module Main where

import           Relude                  hiding ( init )

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact

main :: IO ()
main = play (InWindow "Connect Four" (700, 600) (10, 10))
            black
            0
            init
            mempty
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
