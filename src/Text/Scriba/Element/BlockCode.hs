{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.BlockCode where

import           Text.Scriba.Decorate
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

{- TODO:

- recognize language attributes

-}

newtype BlockCode = BlockCode
  { getBlockCode :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering BlockCode
instance Titling i BlockCode
instance Referencing BlockCode BlockCode
instance Linking BlockCode

blockCodeToText :: BlockCode -> [Text]
blockCodeToText (BlockCode t) = [t]

pBlockCode :: Scriba Element BlockCode
pBlockCode = do
  t <- whileMatchTy "codeBlock" $ allContentOf simpleText
  pure $ BlockCode $ commonIndentStrip $ T.concat t

instance RH.Render BlockCode where
  render (BlockCode t) =
    pure
      $      Html.div
      Html.! HtmlA.class_ "codeBlock"
      $      Html.pre
      $      Html.code
      $      Html.toHtml t
