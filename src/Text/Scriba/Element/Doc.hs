{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Doc where

import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.DocAttrs
import           Text.Scriba.Element.Memoir
import           Text.Scriba.Element.Str        ( HasStr(..) )
import           Text.Scriba.Element.TitleComponent
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

-- | A document with front matter, main matter, and end matter.
data Doc b j i = Doc (DocAttrs j) [Section b i]
  deriving (Eq, Ord, Show, Read, Generic, Functor)

instance HasDocAttrs j (Doc b j i) where
  getDocAttrs (Doc d _) = d

-- * Numbering

-- TODO: Doesn't number anything in the config. Should it?
instance (Numbering a (b i), Numbering a i) => Numbering a (Doc b j i) where
  numbering (Doc da m) = do
    m' <- numbering m
    pure $ Doc da m'

instance (Titling i (b i), Titling i i, FromTitleComponent i) => Titling i (Doc b j i) where
  titling (Doc da m) = do
    m' <- titling m
    pure $ Doc da m'

instance (Referencing i (f a) (g b), Referencing i a b) => Referencing i (Doc f j a) (Doc g j b) where
  referencing (Doc da m) = do
    m' <- referencing m
    pure $ Doc da m'

-- TODO: selectively render empty sections?
-- TODO: Should the title be a Maybe?
instance (RH.Render (b i), RH.Render i, RH.Render j) => RH.Render (Doc b j i) where
  render (Doc t m) = do
    t' <- RH.render $ Heading $ docTitle t
    RH.bumpHeaderDepth $ do
      m' <- RH.render m
      let mlang = HtmlA.lang . Html.toValue <$> docLang t
      pure $ Html.section Html.! HtmlA.class_ "scribaDoc" RH.?? mlang $ do
        Html.header t'
        Html.section Html.! HtmlA.class_ "mainMatter" $ m'

-- * Parsing

-- TODO: have a pSectionNamed :: Text -> Scriba Element Section to
-- deal with special sections, like the matter?
-- TODO: the pBare dm and pExplicitMatter dm thing is a bit bad.
-- TODO: For error purposes it might be better if the meta is in a
-- whileParsing "document meta", and the body is in a whileParsing
-- "document body", but this is somewhat stylistic.
-- TODO: document section config takes precedence over formal block
-- config re: counters, in particular that there is no namespacing
-- going on.
-- TODO: add configuration for elemrel
-- TODO: better math numbering support. The elemrel thing is particularly bad.
-- TODO: the explicit matter parsing is not correct - you should need
-- to specify only a subset of the *Matter
pDoc
  :: HasStr j
  => Scriba Node j
  -> ([j] -> Text)
  -> Scriba Node (b i)
  -> Scriba Node i
  -> Scriba Element (Doc b j i)
pDoc pMetInl stripMarkup pBlk pInl = do
  matchTy "scriba"
  whileParsingElem "scriba" $ do
    dm <- meta $ attrs $ pDocAttrs pMetInl stripMarkup
    content $ pExplicitMatter dm <|> pBare dm
 where
  pMatter t = asNode $ do
    matchTy t
    whileParsingElem t $ allContentOf $ asNode $ pSection pBlk pInl
  pExplicitMatter dm = do
    m <- one $ pMatter "mainMatter"
    zero
    pure $ Doc dm m
  pBare dm = do
    c <- remaining $ asNode $ pSection pBlk pInl
    pure $ Doc dm c
