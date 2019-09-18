module Main where

import           Relude

import           Graphics.Gloss

main :: IO ()
main = 
  play
    (InWindow "Connect Four" (700, 600) (10, 10))
    black
    0
    ()
    mempty
    (const id)
    (const id)

    
