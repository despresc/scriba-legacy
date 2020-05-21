{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Scriba.Element.Memoir where

import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.DocAttrs
import           Text.Scriba.Element.Identifier ( pIdent )
import           Text.Scriba.Element.TitleComponent
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Monad                  ( join
                                                , void
                                                , guard
                                                )
import           Control.Monad.Reader           ( asks )
import           Control.Monad.State            ( gets )
import           Data.Functor                   ( (<&>) )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

{- TODO:

- add books, parts, chapters, perhaps other things

- chapter precis, abstract

-}

-- | A particular document type.

-- TODO: This should probably go in its own module.
data Article b j i = Article
  { articleControlAttrs :: DocAttrs j
  , articleAttrs :: ArticleAttrs j
  , articleFront :: [FrontMatter b i]
  , articleMain :: [Section b i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering a)

instance (Titling i (b i), FromTitleComponent i, Titling i i) => Titling i (Article b j i)
instance (Referencing i (f a) (g b), Referencing i a b) => Referencing i (Article f j a) (Article g j b)

instance (RH.Render (b i), RH.Render i, RH.Render j) => RH.Render (Article b j i) where
  render (Article ca _ f m) = do
    t' <- RH.render $ Heading $ docTitle ca
    RH.bumpHeaderDepth $ do
      f' <- RH.render f
      m' <- RH.render m
      let mlang = HtmlA.lang . Html.toValue <$> docLang ca
      pure $ Html.section Html.! HtmlA.class_ "scribaArticle" RH.?? mlang $ do
        Html.header t'
        Html.section Html.! HtmlA.class_ "frontMatter" $ f'
        Html.section Html.! HtmlA.class_ "mainMatter" $ m'

instance HasDocAttrs j (Article b j i) where
  getDocAttrs = articleControlAttrs

data ArticleAttrs i = ArticleAttrs
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering a, Titling a, Referencing a (ArticleAttrs b))

-- TODO: extend, of course. Might want to modularize?
data FrontMatter b i
  = Foreword [b i]
  | Dedication [b i]
  | Introduction [b i]
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering a)

instance Titling i (b i) => Titling i (FrontMatter b i)
instance (Referencing i (f a) (g b), Referencing i a b) => Referencing i (FrontMatter f a) (FrontMatter g b)
instance (RH.Render (b i), RH.Render i) => RH.Render (FrontMatter b i) where
  render = \case
    Foreword blks -> rfront "forword" blks
    Dedication blks -> rfront "dedication" blks
    Introduction blks -> rfront "introduction" blks
    where
      rfront t x = do
        x' <- RH.render x
        pure $ Html.section Html.! HtmlA.class_ t $ x'

data SecAttrs i = SecAttrs
  { secId :: Maybe Identifier
  , secTitleBody :: Maybe (Title i)
  , secTitleFull :: Maybe (Title i)
  , secNum :: Maybe Text
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

-- | A 'Section' is a major document division in the main matter. It
-- is the top-level section of articles.
data Section b i = Section
  { sectionAttrs :: SecAttrs i
  , sectionPreamble :: [b i]
  , sectionChildren :: [Subsection b i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

data Subsection b i = Subsection
  { subsectionAttrs :: SecAttrs i
  , subsectionContent :: [b i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

newtype Heading i = Heading
  { getHeading :: i
  } deriving (Eq, Ord, Show, Read, Generic, Functor)
    deriving anyclass (Numbering a, Titling a)

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

-- Some kind of deriving via here?
instance (Titling i (b i), Titling i i, FromTitleComponent i) => Titling i (Section b i) where
  titling (Section (SecAttrs mId mtbody mtfull mnum) pre children) = do
    mtbody'   <- titling mtbody
    mtfull'   <- titling mtfull
    pre'      <- titling pre
    children' <- titling children
    mtigen    <- do
      let t = "section"
      mtemplate <- asks $ fmap sconfTitleTemplate . M.lookup t . tcSectionConfig
      pure $ mtemplate <&> \template -> Title <$> runTemplate
        template
        SectionTemplate
        Nothing
        ((: []) . fromTitleNumber <$> mnum)
        (titleBody <$> mtbody)
    pure $ Section
      (SecAttrs mId mtbody' (mtfull' <|> join mtigen <|> mtbody') mnum)
      pre'
      children'

-- TODO: duplication
instance (Titling i (b i), Titling i i, FromTitleComponent i) => Titling i (Subsection b i) where
  titling (Subsection (SecAttrs mId mtbody mtfull mnum) c) = do
    mtbody' <- titling mtbody
    mtfull' <- titling mtfull
    c'      <- titling c
    mtigen  <- do
      let t = "subsection"
      mtemplate <- asks $ fmap sconfTitleTemplate . M.lookup t . tcSectionConfig
      pure $ mtemplate <&> \template -> Title <$> runTemplate
        template
        SectionTemplate
        Nothing
        ((: []) . fromTitleNumber <$> mnum)
        (titleBody <$> mtbody)
    pure $ Subsection
      (SecAttrs mId mtbody' (mtfull' <|> join mtigen <|> mtbody') mnum)
      c'

instance (Numbering a (b i), Numbering a i) => Numbering a (Section b i) where
  numbering (Section (SecAttrs mId tbody tfull mnum) pre child) =
    bracketNumbering (Just "section") mId $ \mnumgen -> do
      tbody' <- numbering tbody
      tfull' <- numbering tfull
      pre'   <- numbering pre
      child' <- numbering child
      pure $ Section (SecAttrs mId tbody' tfull' (mnum <|> mnumgen)) pre' child'

instance (Numbering a (b i), Numbering a i) => Numbering a (Subsection b i) where
  numbering (Subsection (SecAttrs mId tbody tfull mnum) c) =
    bracketNumbering (Just "subsection") mId $ \mnumgen -> do
      tbody' <- numbering tbody
      tfull' <- numbering tfull
      c'     <- numbering c
      pure $ Subsection (SecAttrs mId tbody' tfull' (mnum <|> mnumgen)) c'

instance (Referencing i (f a) (g b), Referencing i a b) => Referencing i (Section f a) (Section g b)

instance (Referencing i (f a) (g b), Referencing i a b) => Referencing i (Subsection f a) (Subsection g b)

instance Referencing i a b => Referencing i (SecAttrs a) (SecAttrs b)

-- TODO: add the section type as a class or data attribute

-- TODO: for untitled sections, perhaps conditionally add an anonymous
-- break? They would be necessary when we first render a sibling
-- untitled section. Some kind of state variable, I think.
instance (RH.Render (b i), RH.Render i) => RH.Render (Section b i) where
  render (Section (SecAttrs ml _ t _) pre c) = do
    t' <- traverse (RH.render . Heading) t
    let ident = (\(Identifier i) -> HtmlA.id (Html.toValue i)) <$> ml
    RH.bumpHeaderDepth $ do
      pre' <- RH.render pre
      c'   <- RH.render c
      let prewrap = Html.div Html.! HtmlA.class_ "sectionPreamble" $ pre'
          cwrap   = Html.div Html.! HtmlA.class_ "sectionContent" $ c'
      pure $ Html.section RH.?? ident $ fromMaybe mempty t' <> prewrap <> cwrap

-- TODO: duplication
instance (RH.Render (b i), RH.Render i) => RH.Render (Subsection b i) where
  render (Subsection (SecAttrs ml _ t _) c) = do
    t' <- traverse (RH.render . Heading) t
    let ident = (\(Identifier i) -> HtmlA.id (Html.toValue i)) <$> ml
    RH.bumpHeaderDepth $ do
      c' <- RH.render c
      let cwrap = Html.div Html.! HtmlA.class_ "sectionPreamble" $ c'
      pure $ Html.section RH.?? ident $ fromMaybe mempty t' <> cwrap

-- * Parsing

--  TODO: more robust parsing here, including digitized vs
--  non-digitized types.
pArticle pMetInl stripMarkup pBlk pInl = do
  matchTy "scriba"
  void $ meta $ attrs $ attr "type" $ content $ do
    consumeWhiteSpace
    t <- one simpleText
    zero
    guard $ t == "article"
  whileParsingElem "scriba" $ do
    dm <- meta $ attrs $ pDocAttrs pMetInl stripMarkup
    content $ pExplicitMatter dm <|> pBare dm
 where
  pExplicitMatter dm = do
    f <- one $ asNode $ pFrontMatter pBlk pInl
    m <- one $ asNode $ pMainMatter pBlk pInl
    pure $ Article dm ArticleAttrs f m
  pBare dm = do
    b <- manyOf pBlk
    c <- remaining $ asNode $ pSection pBlk pInl
    pure $ Article dm ArticleAttrs [Introduction b] c

pFrontMatter pBlk pInl =
  whileMatchTy "frontMatter" $ allContentOf $ asNode $ pFrontMatterSec pBlk pInl

pFrontMatterSec pBlk pInl = pIntro <|> pDedication
 where
  pSectionlike t f = fmap f $ whileMatchTy t $ allContentOf pBlk
  pIntro      = pSectionlike "introduction" Introduction
  pDedication = pSectionlike "dedication" Dedication

pMainMatter pBlk pInl =
  whileMatchTy "mainMatter" $ allContentOf $ asNode $ pSection pBlk pInl

pSecAttrs :: Scriba Node i -> Scriba Attrs (SecAttrs i)
pSecAttrs pInl = do
  mId        <- attrMaybe "id" $ content pIdent
  mtitle     <- attrMaybe "title" $ allContentOf pInl
  mfullTitle <- attrMaybe "fullTitle" $ allContentOf pInl
  mnumber    <- attrMaybe "n" $ allContentOf simpleText
  pure $ SecAttrs mId
                  (Title <$> mtitle)
                  (Title <$> mfullTitle)
                  (T.concat <$> mnumber)

-- For now, all things presented as sections become sections.

-- TODO: do the expectations actually work out here?
-- TODO: a top level title parser?
-- TODO: reduce duplication with pFormalConfig
-- TODO: spin out section attribute parsing.
pSection :: Scriba Node (b i) -> Scriba Node i -> Scriba Element (Section b i)
pSection pBlk pInl = whileMatchTy "section" $ do
  sattr    <- meta $ attrs $ pSecAttrs pInl
  pre      <- content $ manyOf pBlk
  children <- content $ remaining $ asNode $ pSubsection pBlk pInl
  pure $ Section sattr pre children

-- TODO: duplication?
pSubsection
  :: Scriba Node (b i) -> Scriba Node i -> Scriba Element (Subsection b i)
pSubsection pBlk pInl = whileMatchTy "subsection" $ do
  sattr <- meta $ attrs $ pSecAttrs pInl
  c     <- content $ manyOf pBlk
  pure $ Subsection sattr c
