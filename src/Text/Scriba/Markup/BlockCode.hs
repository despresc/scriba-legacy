{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Markup.BlockCode where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype BlockCode = BlockCode
  { getBlockCode :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

blockCodeToText :: BlockCode -> [Text]
blockCodeToText (BlockCode t) = [t]
