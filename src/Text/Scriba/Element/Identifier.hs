{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Identifier
  ( Identifier(..)
  , pIdent
  , prefixIdent
  , identAttr
  , identAttrVal
  )
where

import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Common

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Char                      ( isSpace )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

-- TODO: not sure I want this here. Could just put it in Linking...

-- Parse identifier content without whitespace
-- TODO: some kind of position update fold would be nice here.
pIdent :: Scriba [Node] Identifier
pIdent = do
  t <- T.concat <$> remaining simpleText
  let t' = T.strip t
  when (T.null t') $ throwError $ Msg "identifier must be non-empty"
  when (T.any isSpace t') $ throwError $ Msg
    "identifier cannot have whitespace in it"
  pure $ Identifier t'

prefixIdent :: Text -> Identifier -> Identifier
prefixIdent t (Identifier t') = Identifier (t <> t')

identAttr :: Identifier -> Html.Attribute
identAttr = HtmlA.id . Html.toValue . getIdentifier

identAttrVal :: Identifier -> Html.AttributeValue
identAttrVal = Html.toValue . getIdentifier
