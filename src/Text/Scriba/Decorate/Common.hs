{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Decorate.Common where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype Identifier = Identifier
  { getIdentifier :: Text
  } deriving (Eq, Ord, Show, Read, Generic)
