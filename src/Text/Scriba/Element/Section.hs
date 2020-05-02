{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Scriba.Element.Section where

import           Text.Scriba.Element.TitleComponent
import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( join )
import           Control.Monad.Reader           ( asks )
import           Data.Functor                   ( (<&>) )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import           Data.Traversable               ( for )
import           GHC.Generics                   ( Generic )

-- | A section is a large-scale division of a document. For now it has
-- a preamble and a list of subsections.

-- TODO: maybe preamble isn't the correct name?
-- TODO: the inside should be "section content", probably, and the Doc
-- should have three SectionContent components, since we're enforcing
-- a particular matter structure.
-- TODO: when we do section templating, we may need templates based on
-- the section level.
-- TODO: no section title separator. Doesn't seem hugely necessary
-- right now.
-- TODO: There's no title body or anything here. If a section is
-- configured to be titled then any existing title becomes the
-- body. It doesn't count as an override. Might want configuration for
-- that.

-- TODO: need section titles to have the following behaviour in source:
-- 1. If titleFull is present, use that as the full title.
-- 2. If title is present, put that into titleBody.
-- 3. for title rendering, the precedence for TitleFull should be:
-- title full, generated title, title body
data Section b i = Section
  { secType :: Maybe Text
  , secId :: Maybe Identifier
  , secTitleBody :: Maybe (Title i)
  , secTitleFull :: Maybe (Title i)
  , secNum :: Maybe Text
  , secContent :: SectionContent b i
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

data SectionContent b i = SectionContent
  { secPreamble :: [b i]
  , secChildren :: [Section b i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor, Numbering a)

deriving instance ( FromTitleComponent i
                  , Titling i (b i)
                  , Titling i i
                  ) => Titling i (SectionContent b i)

instance (Referencing i (f a) (g b), Referencing i a b) => Referencing i (SectionContent f a) (SectionContent g b) where

-- TODO: may want to restrict the inlines that can appear in a
-- title. May also want to have a toc title and header/running title
-- in here too. Also may want a richer title structure, say having
-- titles, separators, subtitles, that sort of thing.
-- TODO: Might want this to be in its own module, and create a Heading type as well.
newtype Title i = Title
  { titleBody :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)
    deriving anyclass (Numbering a, Titling a)

instance Referencing i a b => Referencing i (Title a) (Title b)

emptySectionContent :: SectionContent b i
emptySectionContent = SectionContent [] []

-- * Numbering

instance (Numbering a (b i), Numbering a i) => Numbering a (Section b i) where
  numbering (Section mty mId tbody tfull mnum c) =
    bracketNumbering mty mId $ \mnumgen -> do
      tbody' <- numbering tbody
      tfull' <- numbering tfull
      c'     <- numbering c
      pure $ Section mty mId tbody' tfull' (mnum <|> mnumgen) c'

instance (Titling i (b i), Titling i i, FromTitleComponent i) => Titling i (Section b i) where
  titling (Section mty mId mtbody mtfull mnum c) = do
    mtbody' <- titling mtbody
    mtfull' <- titling mtfull
    c'      <- titling c
    mtigen  <- for mty $ \t -> do
      mtemplate <- asks $ fmap sconfTitleTemplate . M.lookup t . tcSectionConfig
      pure $ mtemplate <&> \template -> Title <$> runTemplate
        template
        SectionTemplate
        Nothing
        ((: []) . fromTitleNumber <$> mnum)
        (titleBody <$> mtbody)
    pure $ Section mty
                   mId
                   mtbody'
                   (mtfull' <|> join (join mtigen) <|> mtbody')
                   mnum
                   c'

instance (Referencing i (f a) (g b), Referencing i a b) => Referencing i (Section f a) (Section g b)
