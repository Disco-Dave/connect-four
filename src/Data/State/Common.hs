module Data.State.Common where

import           Prelude

data UpdateResult state
  = UpdateFailed
  | UpdateSuccessful state
  deriving (Show, Eq)

instance Functor UpdateResult where
  fmap _ UpdateFailed         = UpdateFailed
  fmap f (UpdateSuccessful s) = UpdateSuccessful $ f s

instance Applicative UpdateResult where
  pure = UpdateSuccessful
  (UpdateSuccessful f) <*> (UpdateSuccessful s) = UpdateSuccessful $ f s
  _                    <*> _                    = UpdateFailed

instance Monad UpdateResult where
  UpdateSuccessful s >>= f = f s
  _                  >>= _ = UpdateFailed

instance Alternative UpdateResult where
  empty = UpdateFailed
  UpdateFailed <|> r = r
  l            <|> _ = l
