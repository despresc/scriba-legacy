{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Ref where

import           Text.Scriba.Decorate
import           Text.Scriba.Element.Identifier
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Monad.Except           ( MonadError(..) )
import           Data.Functor                   ( ($>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

-- TODO: move this to a common module once it's used elsewhere
-- TODO: zero results in poor error
-- TODO: factor out a "symbol" parser?
-- TODO: this needs to be synchronized with the (currently poor)
-- identifier parser
pRefTarget :: Scriba [Node] RefTarget
pRefTarget = do
  consumeWhiteSpace
  t <- one nodeText
  consumeWhiteSpace
  zero
  case Text.splitOn "." t of
    [x]    -> pure $ RefSelf $ Identifier x
    [x, y] -> pure $ RefQualified (Identifier x) (Identifier y)
    _      -> throwError $ Msg $ "Ill-formed identifier: " <> t

newtype SourceRef = SourceRef
  { sourceRefTarget :: RefTarget
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering SourceRef
instance Titling i SourceRef
instance Gathering note SourceRef SourceRef where
  gathering x@(SourceRef i) = tellReferencedDoc i $> x

-- TODO: may need more renditional information here, from Numbering and
-- Referencing, like relative position of the number and prefix.
-- May also need source overrides on certain elements of this.

-- TODO: For multi-page standalone rendering, will I need to modify
-- identifiers at all? Probably.
data Ref = Ref
  { refTarget :: RefTarget
  , refTargetPrefix :: Text
  , refNumber :: ElemNumber
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering Ref
instance Titling a Ref
instance Referencing Ref Ref
instance Gathering note Ref Ref

-- TODO: Not sure what to do here.
refToText :: (a -> [Text]) -> Ref -> [Text]
refToText _ _ = []

pSourceRef :: Scriba Element SourceRef
pSourceRef = whileMatchTy "ref" $ do
  as <- meta $ args inspect
  case as of
    [t] -> useState [t] $ SourceRef <$> pRefTarget
    _   -> throwError $ Msg "ref takes exactly one identifier as an argument"

-- TODO: error message
resolveRef :: SourceRef -> RefM Ref
resolveRef (SourceRef i) = do
  ld <- lookupRefData i
  case ld of
    LinkNumber t _ en -> pure $ Ref i t en
    _ ->
      throwError
        $  DecorateError
        $  "identifier: <"
        <> refTargetPretty i
        <> "> was used in a reference, but does not have reference capabilities"

-- TODO: wrap separator?
-- TODO: we're special-casing formula for now, so references to math
-- work out. Later we'll want to configure mathjax's tagging.
-- TODO: fix when page data reporting improves!
instance RH.Render Ref where
  render (Ref (RefSelf lab) pref enum) =
    pure
      $      Html.a
      Html.! HtmlA.class_ "ref"
      Html.! HtmlA.href (identAttrVal $ prefixIdent ("#" <> pref) lab)
      $      body
   where
    body = case enum of
      ElemNumberAuto (NumberAuto _ (UsedNumberConfig _ mpref msep) _ num) -> do
        RH.renderMaybe (Html.toHtml <$> mpref) $ Html.span Html.! HtmlA.class_
          "prefix"
        RH.renderMaybe (Html.toHtml <$> msep) id
        Html.span Html.! HtmlA.class_ "number" $ Html.toHtml num
      NumberSource num ->
        Html.span Html.! HtmlA.class_ "number" $ Html.toHtml num
  render (Ref _ _ _) = mempty
