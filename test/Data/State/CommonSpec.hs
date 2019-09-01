{-# LANGUAGE ScopedTypeVariables  #-}

module Data.State.CommonSpec
  ( spec
  )
where

import           Prelude

import           Test.Hspec
import           Test.QuickCheck

import           Data.State.Common


spec :: Spec
spec = describe "UpdateResult" $ do
  it "functor law: identity" $ property $ \(ur :: UpdateResult Int) ->
    fmap id ur `shouldBe` ur

  it "functor law: homomorphism" $ property $ \(ur :: UpdateResult Int) ->
    fmap ((* 2) . (* 3)) ur `shouldBe` (fmap (* 2) . fmap (* 3)) ur

  it "applicative law: identity" $ property $ \(ur :: UpdateResult Int) ->
    pure id <*> ur `shouldBe` ur

  it "applicative law: homomorpism" $
    ((pure (*2) <*> pure 5) :: UpdateResult Int) `shouldBe` (pure (2 * 5) :: UpdateResult Int)

  it "Alternative instance works" $ do
    (empty :: UpdateResult ()) `shouldBe` (UpdateFailed :: UpdateResult ())
    (UpdateSuccessful True <|> UpdateSuccessful False)
      `shouldBe` UpdateSuccessful True
    (UpdateFailed <|> UpdateSuccessful False) `shouldBe` UpdateSuccessful False
  it "Show instance works" $ do
    show (UpdateFailed :: UpdateResult ()) `shouldBe` ("UpdateFailed" :: String)
    show (UpdateSuccessful True) `shouldBe` ("UpdateSuccessful True" :: String)

  it "Eq instance works for UpdateSuccessful constructor when things are equal"
    $ property
    $ \(a :: Int) -> UpdateSuccessful a === UpdateSuccessful a

  it
      "Eq instance works for UpdateSuccessful constructor when things are not equal"
    $ forAll unequalGen
    $ \(a, b) -> UpdateSuccessful a =/= UpdateSuccessful b


  it "Eq instance works for UpdateFailed"
    $          (UpdateFailed :: UpdateResult ())
    `shouldBe` (UpdateFailed :: UpdateResult ())

unequalGen :: Gen (Int, Int)
unequalGen = applyArbitrary2 (,) `suchThat` uncurry (/=)

instance Arbitrary (UpdateResult Int) where
  arbitrary = do
    n <- arbitrary :: Gen Int
    elements [UpdateFailed, UpdateSuccessful n]
