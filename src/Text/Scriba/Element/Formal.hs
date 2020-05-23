{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Formal where

import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.MixedBody
import           Text.Scriba.Element.TitleComponent
import           Text.Scriba.Element.Identifier
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Monad                  ( join )
import           Control.Monad.Reader           ( asks )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Traversable               ( for )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

{- TODO:

- A formal _heading_ title type?

-}

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
  , fLabel :: Maybe Identifier
  , fNum :: Maybe ElemNumber
  , fTitle :: Maybe [i]
  , fNote :: Maybe [i]
  , fTitleSep :: Maybe [i]
  , fContent :: MixedBody b i
  , fConclusion :: Maybe [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

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
-- TODO: I can always just use pMixedBody directly...
pFormal
  :: Scriba [Node] (MixedBody b i)
  -> Scriba [Node] [i]
  -> Scriba Element (Formal b i)
pFormal pBody pInl = whileMatchTy "formalBlock" $ do
  (mty, mId, mnumber, title, note, tsep, concl) <- meta $ attrs $ do
    mty     <- attrMaybe "type" $ allContentOf simpleText
    mId     <- attrMaybe "id" $ content pIdent
    mnumber <- attrMaybe "n" $ allContentOf simpleText
    title   <- attrMaybe "title" $ content pInl
    note    <- attrMaybe "titleNote" $ content pInl
    tsep    <- attrMaybe "titleSep" $ content pInl
    concl   <- attrMaybe "conclusion" $ content pInl
    pure (mty, mId, mnumber, title, note, tsep, concl)
  body <- content pBody
  pure $ Formal (T.concat <$> mty)
                mId
                (NumberSource . T.concat <$> mnumber)
                title
                note
                tsep
                body
                concl

-- * Numbering

-- TODO: we don't skip numbering a formal block when it already has a
-- number. Should have config for that sort of thing.
instance (Numbering (b i), Numbering i) => Numbering (Formal b i) where
  numbering (Formal mty mId mnum ti note tsep cont concl) =
    bracketNumbering mty mId $ \mnumgen -> do
      ti'    <- numbering ti
      note'  <- numbering note
      tsep'  <- numbering tsep
      cont'  <- numbering cont
      concl' <- numbering concl
      pure $ Formal mty mId (mnum <|> mnumgen) ti' note' tsep' cont' concl'

instance (FromTitleComponent i, Titling i (b i), Titling i i) => Titling i (Formal b i) where
  titling (Formal mty mId mnum mti mnote mtisep cont conc) = do
    mti'                         <- titling mti
    mnote'                       <- titling mnote
    mtisep'                      <- titling mtisep
    cont'                        <- titling cont
    conc'                        <- titling conc
    (mtisepgen, mtigen, concgen) <- fmap unzips3 $ for mty $ \t -> do
      mfconf <- asks $ M.lookup t . tcFormalConfig
      pure $ case mfconf of
        Just fconf ->
          let concl    = fconfConcl fconf
              tisep    = fconfTitleSep fconf
              template = fconfTitleTemplate fconf
          in  ( tisep
              , runTemplate template
                            FormalTemplate
                            Nothing
                            ((: []) . fromTitleNumber . elemNumberNum <$> mnum)
                            mnote
              , concl
              )
        Nothing -> (Nothing, Nothing, Nothing)
    pure $ Formal mty
                  mId
                  mnum
                  (mti' <|> join mtigen)
                  mnote'
                  (mtisep' <|> join mtisepgen)
                  cont'
                  (conc' <|> join concgen)

instance (Referencing (f a) (g b), Referencing a b) => Referencing (Formal f a) (Formal g b)

-- TODO: add the type as a data-scribaType? Though we might want that
-- to equal formalBlock here. Might want to record the number as well.
-- TODO: Should the title be "formalTitle"?
-- TODO: wrap the title separator? Also the title separator should be
-- rendered conditional on there being a title at all.
-- TODO: Have something in the margin indicating an anchor point?
instance (RH.Render (b i), RH.Render i) => RH.Render (Formal b i) where
  render (Formal mty ml _ mtitle _ mtitlesep body concl) = do
    title'    <- traverse RH.render mtitle
    titlesep' <- traverse RH.render mtitlesep
    body'     <- RH.render body
    concl'    <- traverse RH.render concl
    let cls   = "formalBlock" <> maybe "" (" " <>) mty
        ident = (\(Identifier i) -> HtmlA.id (Html.toValue i)) <$> ml
    pure $ Html.div Html.! HtmlA.class_ (Html.toValue cls) RH.?? ident $ do
      RH.renderMaybe title' $ Html.span Html.! HtmlA.class_ "title"
      RH.renderMaybe titlesep' $ Html.span Html.! HtmlA.class_ "titleSep"
      body'
      RH.renderMaybe concl' $ Html.span Html.! HtmlA.class_ "conclusion"
