module View.Gloss
  ( render
  )
where

import           Prelude
import           Graphics.Gloss
import           Data.State.Game                ( GameState )

render :: GameState -> Picture
render _ = Pictures
  [ Translate (-100) 0 $ Color black $ circleSolid 40
  , Translate (-200) 0 $ Color black $ circleSolid 40
  , Translate (-300) 0 $ Color black $ circleSolid 40
  , Translate 0 0 $ Color black $ circleSolid 40
  , Translate 100 0 $ Color black $ circleSolid 40
  , Translate 200 0 $ Color black $ circleSolid 40
  , Translate 300 0 $ Color black $ circleSolid 40

  , Translate (-100) (-250) $ Color red $ circleSolid 40
  , Translate (-200) (-150) $ Color blue $ circleSolid 40
  , Translate (-300) 0 $ Color black $ circleSolid 40
  , Translate 0 0 $ Color black $ circleSolid 40
  , Translate 100 0 $ Color black $ circleSolid 40
  , Translate 200 0 $ Color black $ circleSolid 40
  , Translate 300 0 $ Color black $ circleSolid 40
  ]

