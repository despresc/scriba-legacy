{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Scriba.Markup.Formal where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Text.Scriba.Markup.MixedBody

-- Might want a formal inline too. Some kind of "inline result",
-- anyway.
-- TODO: the fTitle _might_ be better as Title, but I'm not sure if a
-- formalBlock title should be the same thing as a section title.
-- TODO: Maybe title should be a maybe...
-- TODO: Might want the note to be exclusive with title? We could have
-- Formal be polymorphic in its meta, then have the title be Either
-- FullTitle Note, then decorate can turn that into a FullTitle
-- TODO: Make the conclusion a maybe and add conclusion setting to the formal config.
data Formal b i = Formal
  { fType :: Maybe Text
  , fNum :: Maybe Text
  , fTitle :: Maybe [i]
  , fNote :: Maybe [i]
  , fTitleSep :: Maybe [i]
  , fContent :: MixedBody b i
  , fConclusion :: Maybe [i]
  } deriving (Eq, Ord, Show, Read, Generic)
