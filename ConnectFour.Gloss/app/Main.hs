module Main where

import           Relude                  hiding ( init )

import           Graphics.Gloss
import           Lib

main :: IO ()
main = play (InWindow "Connect Four" (700, 660) (10, 10))
            black
            0
            init
            drawPicture
            handleEvent
            (const id)
