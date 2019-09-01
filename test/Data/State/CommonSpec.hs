{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ViewPatterns  #-}

module Data.State.CommonSpec
  ( spec
  )
where

import           Prelude

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Function

import           Data.State.Common


spec :: Spec
spec = describe "UpdateResult" $ do
  it "functor law: identity" $ property $ \(ur :: UpdateResult Int) ->
    fmap id ur `shouldBe` ur

  it "functor law: composition" $ property $ 
    \FunctorData{updateResult = ur, functor1 = (apply -> f), functor2 = (apply -> g)} ->
      fmap (g . f) ur === (fmap g . fmap f $ ur)
         
  it "applicative law: identity" $ property $ \(ur :: UpdateResult Int) ->
    pure id <*> ur `shouldBe` ur

  it "monad law: right identity" $ property $ \(ur :: UpdateResult Int) ->
    (ur >>= return) === ur

  it "monad law: left identity" $ property $ \MonadData{mPlainValue = v, mFunction1 = (apply -> f)} ->
    (return v >>= f) === (f v)

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

instance Arbitrary (UpdateResult Bool) where
  arbitrary = do
    n <- arbitrary :: Gen Bool
    elements [UpdateFailed, UpdateSuccessful n]

data FunctorData = FunctorData
  { updateResult :: UpdateResult Int
  , functor1 :: Fun Int Bool
  , functor2 :: Fun Bool Int
  } deriving Show

instance Arbitrary FunctorData where
  arbitrary =
    FunctorData
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

data ApplicativeData = ApplicativeData
  { aValue :: Int
  , aFunction :: Fun Int String
  } deriving Show

instance Arbitrary ApplicativeData where
  arbitrary =
    ApplicativeData
      <$> arbitrary
      <*> arbitrary

data MonadData = MonadData
  { mUpdateResult :: UpdateResult Int
  , mPlainValue :: Int
  , mFunction1 :: Fun Int (UpdateResult Bool)
  , mFunction2 :: Fun Bool (UpdateResult Int)
  } deriving Show

instance Arbitrary MonadData where
  arbitrary =
    MonadData
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
