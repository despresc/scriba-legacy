{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.MixedBody where

import           Text.Scriba.Decorate
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

data MixedBody b i
  = MixedInline [i]
  | MixedBlock [b i]
  deriving (Eq, Ord, Show, Read, Generic, Functor, Numbering, Titling a, Gathering note)

instance ( Referencing (f a) (g b)
         , Referencing a b
         ) => Referencing (MixedBody f a) (MixedBody g b)

-- TODO: This does conditional span/div rendering. Is that robust?

-- TODO: This used to do conditional wrapping with a Bool
-- parameter. Could always re-introduce that with another Render
-- instance.
instance (RH.Render (b i), RH.Render i) => RH.Render (MixedBody b i) where
  render (MixedInline ils) =
    Html.span Html.! HtmlA.class_ "body" <$> RH.render ils
  render (MixedBlock blks) =
    Html.div Html.! HtmlA.class_ "body" <$> RH.render blks

pBlockBody :: Scriba Node (b i) -> Scriba [Node] [b i]
pBlockBody = remaining

pInlineBody :: Scriba Node a -> Scriba [Node] [a]
pInlineBody = remaining

pMixedBody
  :: Scriba Node (b i) -> Scriba Node i -> Scriba [Node] (MixedBody b i)
pMixedBody pBlk pInl =
  MixedBlock <$> pBlockBody pBlk <|> MixedInline <$> pInlineBody pInl
