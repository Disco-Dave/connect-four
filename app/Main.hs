module Main where

import           Prelude
import           Graphics.Gloss
import           View.Gloss                     ( render )
import qualified Data.State.Game               as GameState

main :: IO ()
main = play (InWindow "Connect Four" (700, 600) (10, 10))
            yellow
            0
            GameState.init
            render
            (\e s -> traceShow e s)
            (const id)



picture :: Picture
picture = Translate 100 200 (Circle 80) <> Translate 10 10 (Circle 10)


