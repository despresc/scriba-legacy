{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Ref where

import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Linking
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.Identifier
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Monad.Except           ( MonadError(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

newtype SourceRef = SourceRef
  { sourceRefTarget :: Identifier
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering SourceRef
instance Titling i SourceRef
instance Linking SourceRef

-- TODO: may need more renditional information here, from Numbering and
-- Referencing, like relative position of the number and prefix.
-- May also need source overrides on certain elements of this.

-- TODO: For multi-page standalone rendering, will I need to modify
-- identifiers at all? Probably.
data Ref = Ref
  { refTarget :: Identifier
  , refTargetPrefix :: Text
  , refNumber :: ElemNumber
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering Ref
instance Titling a Ref
instance Referencing Ref Ref
instance Linking Ref

-- TODO: Not sure what to do here.
refToText :: (a -> [Text]) -> Ref -> [Text]
refToText _ _ = []

pSourceRef :: Scriba Element SourceRef
pSourceRef = whileMatchTy "ref" $ do
  as <- meta $ args inspect
  case as of
    [t] -> useState [t] $ SourceRef <$> pIdent
    _   -> throwError $ Msg "ref takes exactly one identifier as an argument"

resolveRef :: SourceRef -> RefM Ref
resolveRef (SourceRef i) = do
  (t, en) <- lookupRefData i
  pure $ Ref i t en

-- TODO: wrap separator?
-- TODO: we're special-casing formula for now, so references to math
-- work out. Later we'll want to configure mathjax's tagging.
instance RH.Render Ref where
  render (Ref (Identifier lab) pref enum) =
    pure
      $      Html.a
      Html.! HtmlA.class_ "ref"
      Html.! HtmlA.href (Html.toValue $ "#" <> pref <> lab)
      $      body
   where
    body = case enum of
      NumberAuto _ (UsedNumberConfig _ mpref msep) num -> do
        RH.renderMaybe (Html.toHtml <$> mpref) $ Html.span Html.! HtmlA.class_
          "prefix"
        RH.renderMaybe (Html.toHtml <$> msep) id
        Html.span Html.! HtmlA.class_ "number" $ Html.toHtml num
      NumberSource num ->
        Html.span Html.! HtmlA.class_ "number" $ Html.toHtml num
