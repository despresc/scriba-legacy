{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Section where

import Text.Scriba.Numbering

import Control.Applicative ((<|>))
import           Data.Text                      ( Text )
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
  , secTitleBody :: Maybe (Title i)
  , secTitleFull :: Maybe (Title i)
  , secNum :: Maybe Text
  , secContent :: SectionContent b i
  } deriving (Eq, Ord, Show, Read, Generic)

data SectionContent b i = SectionContent
  { secPreamble :: [b i]
  , secChildren :: [Section b i]
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: may want to restrict the inlines that can appear in a
-- title. May also want to have a toc title and header/running title
-- in here too. Also may want a richer title structure, say having
-- titles, separators, subtitles, that sort of thing.
-- TODO: Might want this to be in its own module, and create a Heading type as well.
newtype Title a = Title
  { titleBody :: [a]
  } deriving (Eq, Ord, Show, Read, Generic)

emptySectionContent :: SectionContent b i
emptySectionContent = SectionContent [] []

-- * Numbering

numTitle :: Numbers [i] -> Numbers (Title i)
numTitle f (Title i) = Title <$> f i

numSectionContent
  :: Numbers [b i]
  -> Numbers [i]
  -> Numbers (SectionContent b i)
numSectionContent numBlocks numInls (SectionContent p c) = do
  p' <- numBlocks p
  c' <- traverse (numSection numBlocks numInls) c
  pure $ SectionContent p' c'

numSection :: Numbers [b i] -> Numbers [i] -> Numbers (Section b i)
numSection numBlocks numInls (Section mty tbody tfull mnum c) =
  bracketNumbering mty $ \mnumgen -> do
    tbody' <- traverse (numTitle numInls) tbody
    tfull' <- traverse (numTitle numInls) tfull
    c'     <- numSectionContent numBlocks numInls c
    pure $ Section mty tbody' tfull' (mnum <|> mnumgen) c'
