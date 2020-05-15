{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Doc where

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.Section
import           Text.Scriba.Element.TitleComponent
import qualified Text.Scriba.Render.Html       as RH

import           Data.Map.Strict                ( Map )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

{- TODO:

- split this further. The titling config and numbering config should
  be bundled and put in a separate module.

- improve runTemplate

-}

-- | A document with front matter, main matter, and end matter.

-- TODO: have a separate index for the attrs, so that they can have Void arguments?
data Doc b j i = Doc (DocAttrs j) (SectionContent b i) (SectionContent b i) (SectionContent b i)
  deriving (Eq, Ord, Show, Read, Generic, Functor)

-- TODO: should I mapKey the docNumberStyle here?

-- TODO: I need the numberconfig to have something like Void type, for
-- now. Otherwise I need to resolve control elements inside prefixes and things
data DocAttrs i = DocAttrs
  { docTitle :: Title i
  , docPlainTitle :: Text
  , docTitlingConfig :: TitlingConfig i
  , docElemCounterRel :: Map ContainerName (CounterName, NumberConfig i)
  , docCounterRel :: Map CounterName (Set CounterName)
  , docMathMacros :: Map Text (Int, Text)
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

emptySurround :: Surround a
emptySurround = Surround [] Nothing []

-- * Numbering

-- TODO: Doesn't number anything in the config. Should it?
instance (Numbering a (b i), Numbering a i) => Numbering a (Doc b j i) where
  numbering (Doc da f m b) = do
    f' <- numbering f
    m' <- numbering m
    b' <- numbering b
    pure $ Doc da f' m' b'

instance (Titling i (b i), Titling i i, FromTitleComponent i) => Titling i (Doc b j i) where
  titling (Doc da f m b) = do
    f' <- titling f
    m' <- titling m
    b' <- titling b
    pure $ Doc da f' m' b'

instance (Referencing i (f a) (g b), Referencing i a b) => Referencing i (Doc f j a) (Doc g j b) where
  referencing (Doc da f m b) = do
    f' <- referencing f
    m' <- referencing m
    b' <- referencing b
    pure $ Doc da f' m' b'

-- TODO: selectively render empty sections?
-- TODO: Should the title be a Maybe?
instance (RH.Render (b i), RH.Render i, RH.Render j) => RH.Render (Doc b j i) where
  render (Doc t f m b) = do
    t' <- RH.render $ Heading $ docTitle t
    RH.bumpHeaderDepth $ do
      f' <- RH.render f
      m' <- RH.render m
      b' <- RH.render b
      pure $ Html.section Html.! HtmlA.class_ "scribaDoc" $ do
        Html.header t'
        Html.section Html.! HtmlA.class_ "frontMatter" $ f'
        Html.section Html.! HtmlA.class_ "mainMatter" $ m'
        Html.section Html.! HtmlA.class_ "backMatter" $ b'
