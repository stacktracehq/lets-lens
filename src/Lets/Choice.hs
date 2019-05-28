{-# LANGUAGE InstanceSigs #-}

module Lets.Choice
  ( Choice(..)
  )
where

import           Lets.Data
import           Lets.Profunctor
import           Data.Bifunctor                as BF

diswap
  :: Profunctor p => p (Either a b) (Either c d) -> p (Either b a) (Either d c)
diswap = dimap swap swap where swap = either Right Left

-- | Map on left or right of @Either@. Only one of @left@ or @right@ needs to be
-- provided.
class Profunctor p => Choice p where
  left ::
    p a b
    -> p (Either a c) (Either b c)
  left =
    diswap . right

  right ::
    p a b
    -> p (Either c a) (Either c b)
  right =
    diswap . left

instance Choice (->) where
  left :: (a -> b) -> (Either a c -> Either b c)
  left = BF.first
  right :: (a -> b) -> (Either c a -> Either c b)
  right = BF.second

instance Choice Tagged where
  left :: Tagged a b -> Tagged (Either a c) (Either b c)
  left (Tagged b) = Tagged (Left b)
  right :: Tagged a b -> Tagged (Either c a) (Either c b)
  right (Tagged b) = Tagged (Right b)

