module DustUp.Artifact where

import Data.ByteString (ByteString)
import DustUp.Dice (Category, Dice)

data Artifact ability
  = ColumnOne
      { speed :: Int
      , will :: Int
      , name :: ByteString
      , tag :: ByteString
      , left :: ability
      , right :: ability
      }
  | ColumnTwo
      { distribution
          :: ( Category
             , Category
             , Category
             , Category
             , Category
             , Category
             )
      , name :: ByteString
      , tag :: ByteString
      , left :: ability
      , right :: ability
      }
  | ColumnThree
      { life :: Int
      , capability :: Int
      , name :: ByteString
      , charge :: ability
      , sp :: ability
      }
