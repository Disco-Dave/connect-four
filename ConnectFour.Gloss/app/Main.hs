{-# LANGUAGE RecordWildCards #-}

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

init :: World
init = World

data World = World

handleEvent :: Event -> World -> World
handleEvent _ v = v


{- VIEW CODE -}

drawPicture :: World -> Picture
drawPicture world = Pictures [drawBoard world, drawCommandBar world]


drawBoard :: World -> Picture
drawBoard _ = translate 0 (-30) $ color yellow $ rectangleSolid 700 600

drawCommandBar :: World -> Picture
drawCommandBar _ = translate 0 300 $ color red $ rectangleSolid 700 60
