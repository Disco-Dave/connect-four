module Main where

import           Prelude
import           Graphics.Gloss

main :: IO ()
main = display (InWindow "Nice Window" (700, 600) (10, 10)) white picture

picture :: Picture
picture = Translate 100 200 (Circle 80) <> Translate 10 10 (Circle 10)
