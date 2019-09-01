module Data.State.BoardSpec
  ( spec
  )
where

import           Prelude
import           Test.Hspec
import           Data.Maybe                     ( isNothing
                                                , isJust
                                                )
import qualified Data.State.Board              as Board
import qualified Data.Array                    as Array

spec :: Spec
spec = do
  describe "init" $ it "creates an empty board" $ do
    let board = Board.init
        columns =
          (flip Board.column board <$> enumFrom minBound) >>= Array.elems
    all isNothing columns `shouldBe` True

  describe "dropInto" $ do
    it "can drop a chip into every column"
      $ for_ (enumFrom Board.ColumnOne) $ \column -> 
        for_ [Board.RedChip, Board.BlueChip] $ \chip -> do
          let board = Board.init & Board.dropInto chip column
          case board of
            Board.UpdateFailed       -> fail "Update should of not failed."
            Board.UpdateSuccessful b -> do
              let slot = Board.slot Board.RowSix column b
              slot `shouldBe` Just chip

    it "can drop no more than six chips in a column"
      $ for_ (enumFrom Board.ColumnOne) $ \column -> do
          let dropChipM b c = b >>= Board.dropInto c column
          for_ [Board.RedChip, Board.BlueChip] $ \chip -> do
            let board = foldl' dropChipM (pure Board.init) (replicate 6 chip)
            case board of
              Board.UpdateFailed       -> fail "Update should of not failed"
              Board.UpdateSuccessful b -> do
                let boardColumn = Board.column column b
                all isJust boardColumn `shouldBe` True
                Board.dropInto chip column b `shouldBe` Board.UpdateFailed
