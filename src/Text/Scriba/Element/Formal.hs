{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Formal where

import           Text.Scriba.Element.MixedBody
import           Text.Scriba.Intermediate
import Text.Scriba.Numbering

import           Data.Text                      ( Text )
import qualified Data.Text as T
import           GHC.Generics                   ( Generic )


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


-- TODO: no formal block type validation
-- TODO: sort of a hack allowing simple inline content: we just wrap
-- bare content in a paragraph. Might want a Plain-type block after
-- all?  Some kind of reusable thing that signals that the content of
-- a block can be bare.

-- TODO: For rendering, consider whether the title or conclusion
-- should be inserted inside the body! E.g. if the first block in the
-- FormalBlock is a paragraph, perhaps we should put that in the
-- paragraph? Might not be necessary with something like the "display:
-- inline" property on the first paragraph (or first paragraph after
-- the title).

-- TODO: In the body parser I formerly had a single allContent $
-- ... invocation, with the choice inside. That didn't work, because
-- the manyOf can always succeed. Maybe I can preserve the behaviour
-- by having the first one be a someOf?

pFormal
  :: Scriba [Node] (MixedBody b i)
  -> Scriba [Node] [i]
  -> Scriba Element (Formal b i)
pFormal pBody pInl = whileMatchTy "formalBlock" $ do
  (mty, mnumber, title, note, tsep, concl) <- meta $ attrs $ do
    mty     <- attrMaybe "type" $ allContentOf simpleText
    mnumber <- attrMaybe "n" $ allContentOf simpleText
    title   <- attrMaybe "title" $ content pInl
    note    <- attrMaybe "titleNote" $ content pInl
    tsep    <- attrMaybe "titleSep" $ content pInl
    concl   <- attrMaybe "conclusion" $ content pInl
    pure (mty, mnumber, title, note, tsep, concl)
  body <- content pBody
  pure $ Formal (T.concat <$> mty)
                (T.concat <$> mnumber)
                title
                note
                tsep
                body
                concl

-- * Numbering

-- TODO: we don't skip numbering a formal block when it already has a
-- number. Should have config for that sort of thing.
numFormal :: Numbers (MixedBody b i) -> Numbers [i] -> Numbers (Formal b i)
numFormal numBody numInls (Formal mty mnum ti note tsep cont concl) =
  bracketNumbering mty $ \mnumgen -> do
    ti'    <- traverse numInls ti
    note'  <- traverse numInls note
    tsep'  <- traverse numInls tsep
    cont'  <- numBody cont
    concl' <- traverse numInls concl
    pure $ Formal mty (mnum <|> mnumgen) ti' note' tsep' cont' concl'
