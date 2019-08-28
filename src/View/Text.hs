-- | A view that can render the board to Text
module View.Text
  ( render
  )
where

import           Prelude
import           Data.State.Board               ( Board
                                                , Chip(..)
                                                )
import qualified Data.State.Board              as Board

-- | Renders the board to Text
render :: Board -> Text
render board = foldMap ((<> "\n") . renderRow') $ enumFrom minBound
  where renderRow' = renderRow . flip Board.row board

renderChip :: Maybe Chip -> Text
renderChip Nothing         = "[ ]"
renderChip (Just RedChip ) = "[R]"
renderChip (Just BlueChip) = "[B]"

renderRow :: Traversable t => t (Maybe Chip) -> Text
renderRow = foldMap ((<> " ") . renderChip)
