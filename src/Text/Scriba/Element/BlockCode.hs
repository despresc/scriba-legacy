{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.BlockCode where

import           Text.Scriba.Intermediate
import           Text.Scriba.Numbering
import           Text.Scriba.Titling

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

{- TODO:

- recognize language attributes

-}

newtype BlockCode = BlockCode
  { getBlockCode :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering BlockCode
instance Titling i BlockCode

blockCodeToText :: BlockCode -> [Text]
blockCodeToText (BlockCode t) = [t]

pBlockCode :: Scriba Element BlockCode
pBlockCode = do
  t <- whileMatchTy "codeBlock" $ allContentOf simpleText
  pure $ BlockCode $ commonIndentStrip $ T.concat t
