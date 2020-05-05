{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.BlockCode where

import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

{- TODO:

- recognize language attributes

-}

newtype BlockCode = BlockCode
  { getBlockCode :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering i BlockCode
instance Titling i BlockCode
instance Referencing i BlockCode BlockCode

blockCodeToText :: BlockCode -> [Text]
blockCodeToText (BlockCode t) = [t]

pBlockCode :: Scriba Element BlockCode
pBlockCode = do
  t <- whileMatchTy "codeBlock" $ allContentOf simpleText
  pure $ BlockCode $ commonIndentStrip $ T.concat t
