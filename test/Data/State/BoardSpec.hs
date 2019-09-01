module Data.State.BoardSpec
  ( spec
  )
where

import           Prelude
import           Test.Hspec
import           Data.Maybe                     ( isNothing )
import qualified Data.State.Board              as Board
import qualified Data.Array                    as Array

spec :: Spec
spec = describe "init" $ it "creates an empty board" $ do
  let board   = Board.init
      columns = (flip Board.column board <$> enumFrom minBound) >>= Array.elems
  all isNothing columns `shouldBe` True
