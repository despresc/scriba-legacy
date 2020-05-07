{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Ref where

import           Text.Scriba.Counters
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

instance Numbering i SourceRef
instance Titling i SourceRef

-- TODO: may need more renditional information here, from Numbering and
-- Referencing, like relative position of the number and prefix.
-- May also need source overrides on certain elements of this.

-- TODO: For multi-page standalone rendering, will I need to modify
-- identifiers at all? Probably.
data Ref i = Ref
  { refTarget :: Identifier
  , refContainer :: ContainerName
  , refNumberConfig :: UsedNumberConfig i
  , refNumber :: Text
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

instance Numbering a i => Numbering a (Ref i)
instance Titling a i => Titling a (Ref i)
instance Referencing i a b => Referencing i (Ref a) (Ref b)

-- TODO: Not sure what to do here.
refToText :: (a -> [Text]) -> Ref a -> [Text]
refToText _ _ = []

pSourceRef :: Scriba Element SourceRef
pSourceRef = whileMatchTy "ref" $ do
  as <- meta $ args inspect
  case as of
    [t] -> useState [t] $ SourceRef <$> pIdent
    _   -> throwError $ Msg "ref takes exactly one identifier as an argument"

resolveRef :: SourceRef -> RefM i (Ref i)
resolveRef (SourceRef i) = do
  (cn, nc, num) <- lookupRefData i
  pure $ Ref i cn nc num

-- TODO: wrap separator?
-- TODO: we're special-casing formula for now, so references to math
-- work out. Later we'll want to configure mathjax's tagging.
instance RH.Render a => RH.Render (Ref a) where
  render (Ref (Identifier lab) containername (UsedNumberConfig _ mpref msep) num)
    = do
      mpref' <- traverse RH.render mpref
      msep'  <- traverse RH.render msep
      pure
        $      Html.a
        Html.! HtmlA.class_ "ref"
        Html.! HtmlA.href (Html.toValue refVal)
        $      do
                 RH.renderMaybe mpref' $ Html.span Html.! HtmlA.class_ "prefix"
                 RH.renderMaybe msep' id
                 Html.span Html.! HtmlA.class_ "number" $ Html.toHtml num
   where
    refVal = case getContainerName containername of
      "formula" -> "#mjx-eqn-" <> lab
      _         -> "#" <> lab

