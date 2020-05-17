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

import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.Identifier ( pIdent )
import           Text.Scriba.Element.TitleComponent
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Monad                  ( join )
import           Control.Monad.Reader           ( asks )
import           Control.Monad.State            ( gets )
import           Data.Functor                   ( (<&>)
                                                , ($>)
                                                )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Traversable               ( for )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

{- TODO:

- a mode that allows us to style untyped sections based on level,
  perhaps in a similar way to the "list context" facilities we will
  implement

- consider unifying formal blocks and sections together, at least
  internally. They are quite similar. Perhaps the "conclusion" of
  formal blocks is not so necessary, especially since its automatic
  placement can be difficult. In such a scenario, we'd end up calling
  them "sections", but we'd need to distinguish between "large-scale"
  divisions that are suitable for a table of contents and lesser
  divisions.

-}

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

newtype Heading i = Heading
  { getHeading :: i
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

-- TODO: add the section type as a class or data attribute

-- TODO: for untitled sections, perhaps conditionally add an anonymous
-- break? They would be necessary when we first render a sibling
-- untitled section. Some kind of state variable, I think.
instance (RH.Render (b i), RH.Render i) => RH.Render (Section b i) where
  render (Section _ ml _ t _ c) = do
    t' <- traverse (RH.render . Heading) t
    let ident = (\(Identifier i) -> HtmlA.id (Html.toValue i)) <$> ml
    RH.bumpHeaderDepth $ do
      c' <- RH.render c
      pure $ Html.section RH.?? ident $ fromMaybe mempty t' <> c'

-- TODO: Distinguish the preamble from the subsections?
instance (RH.Render (b i), RH.Render i) => RH.Render (SectionContent b i) where
  render (SectionContent bs cs) = do
    bs' <- RH.render bs
    cs' <- RH.render cs
    pure $ Html.div Html.! HtmlA.class_ "sectionContent" $ bs' <> cs'

-- Add a sectionTitle class?
instance RH.Render i => RH.Render (Title i) where
  render (Title t) = do
    t' <- RH.render t
    pure $ Html.span Html.! HtmlA.class_ "title" $ t'

instance RH.Render i => RH.Render (Heading i) where
  render (Heading t) = do
    lvl <- gets RH.rsHeaderDepth
    headAtLevel lvl <$> RH.render t
   where
    headAtLevel n = case n of
      1 -> Html.h1
      2 -> Html.h2
      3 -> Html.h3
      4 -> Html.h4
      5 -> Html.h5
      _ -> Html.h6

-- * Parsing


-- For now, all things presented as sections become sections.

-- TODO: do the expectations actually work out here?
-- TODO: a top level title parser?
-- TODO: reduce duplication with pFormalConfig
pSection :: Scriba Node (b i) -> Scriba Node i -> Scriba Element (Section b i)
pSection pBlk pInl = do
  mty <- (matchTy "section" $> Just "section") <|> presentedAsSection
  whileParsingElem (fromMaybe "section of unknown type" mty) $ do
    (mId, mtitle, mfullTitle, mnumber) <- meta $ attrs $ do
      mId        <- attrMaybe "id" $ content pIdent
      mtitle     <- attrMaybe "title" $ allContentOf pInl
      mfullTitle <- attrMaybe "fullTitle" $ allContentOf pInl
      mnumber    <- attrMaybe "n" $ allContentOf simpleText
      pure (mId, mtitle, mfullTitle, mnumber)
    c <- content $ pSectionContent pBlk pInl
    pure $ Section mty
                   mId
                   (Title <$> mtitle)
                   (Title <$> mfullTitle)
                   (T.concat <$> mnumber)
                   c
 where
  presentedAsSection = do
    meta $ do
      Meta _ pres _ _ <- inspect
      case pres of
        AsSection _ -> pure ()
        _           -> empty
    ty inspect

pSectionContent
  :: Scriba Node (b i) -> Scriba Node i -> Scriba [Node] (SectionContent b i)
pSectionContent pBlk pInl = do
  pre  <- manyOf pBlk
  subs <- remaining $ asNode (pSection pBlk pInl)
  pure $ SectionContent pre subs
