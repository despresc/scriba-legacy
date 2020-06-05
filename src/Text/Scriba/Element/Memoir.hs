{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Memoir where

import           Text.Scriba.Decorate
import           Text.Scriba.Element.DocAttrs
import           Text.Scriba.Element.Identifier
import           Text.Scriba.Element.Str        ( HasStr )
import           Text.Scriba.Element.Title      ( Title(..) )
import           Text.Scriba.Element.TitleComponent
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Monad                  ( join )
import           Control.Monad.Except           ( throwError )
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

- some sort of section-like parser? to cut down on duplication

- section type inference? We know that in articles the children of
  mainMatter must be sections, for instance. Would have to change once
  we allow more types of documents and more types of mainMatter
  sections, but we can still infer based on the resulting structure
  (span of sections, then span of appendices, say).

-}

data Doc b j i = Doc
  { docLibAttrs :: ()
  , docContent :: DocContent b j i
  } deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering)

instance (Titling i (b i), FromTitleComponent i, Titling i i) => Titling i (Doc b j i)
instance (Gathering note (f a) (g b), Gathering note a b) => Gathering note (Doc f j a) (Doc g j b)
instance HasDocAttrs j (Doc b j i) where
  getDocAttrs (Doc _ (DocArticle a)) = getDocAttrs a
instance (Referencing (f a) (g b), Referencing a b) => Referencing (Doc f j a) (Doc g j b)
instance (RH.Render (b i), RH.Render i, RH.Render j) => RH.Render (Doc b j i) where
  render (Doc _ (DocArticle a)) = RH.render a

data DocContent b j i
  = DocArticle (Article b j i)
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering)

instance (Gathering note (f a) (g b), Gathering note a b) => Gathering note (DocContent f j a) (DocContent g j b)
instance (Titling i (b i), FromTitleComponent i, Titling i i) => Titling i (DocContent b j i)
instance (Referencing (f a) (g b), Referencing a b) => Referencing (DocContent f j a) (DocContent g j b)

-- | A particular document type.

-- TODO: This should probably go in its own module.
data Article b j i = Article
  { articleControlAttrs :: DocAttrs j
  , articleAttrs :: ArticleAttrs j
  , articleFront :: [FrontMatter b i]
  , articleMain :: [Section b i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering)

instance (Titling i (b i), FromTitleComponent i, Titling i i) => Titling i (Article b j i)
instance (Referencing (f a) (g b), Referencing a b) => Referencing (Article f j a) (Article g j b)

instance (Gathering note (f a) (g b), Gathering note a b) => Gathering note (Article f j a) (Article g j b)

instance (RH.Render (b i), RH.Render i, RH.Render j) => RH.Render (Article b j i) where
  render (Article ca _ f m) = do
    t' <- RH.render $ Heading $ docTitle ca
    RH.bumpHeaderDepth $ do
      f' <- RH.render f
      m' <- RH.render m
      let mlang = HtmlA.lang . Html.toValue <$> docLang ca
      pure
        $      Html.section
        Html.! HtmlA.class_ "scribaDoc article"
        RH.??  mlang
        $      do
                 Html.header t'
                 Html.section Html.! HtmlA.class_ "frontMatter" $ f'
                 Html.section Html.! HtmlA.class_ "mainMatter" $ m'

instance HasDocAttrs j (Article b j i) where
  getDocAttrs = articleControlAttrs

data ArticleAttrs i = ArticleAttrs
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering, Titling a, Referencing (ArticleAttrs b), Gathering note (ArticleAttrs b))

-- TODO: extend, of course. Might want to modularize?
data FrontMatter b i
  = Foreword [b i]
  | Dedication [b i]
  | Introduction [b i]
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering)

instance Titling i (b i) => Titling i (FrontMatter b i)
instance ( Referencing (f a) (g b)
         , Referencing a b
         ) => Referencing (FrontMatter f a) (FrontMatter g b)
instance ( Gathering note (f a) (g b)
         , Gathering note a b
         ) => Gathering note (FrontMatter f a) (FrontMatter g b)
instance (RH.Render (b i), RH.Render i) => RH.Render (FrontMatter b i) where
  render = \case
    Foreword     blks -> rfront "foreword" blks
    Dedication   blks -> rfront "dedication" blks
    Introduction blks -> rfront "introduction" blks
   where
    rfront t x = do
      x' <- RH.render x
      pure $ Html.section Html.! HtmlA.class_ t $ x'

-- TODO: document section title behaviour:
-- 1. If titleFull is present, use that as the full title.
-- 2. If title is present, put that into titleBody.
-- 3. for title rendering, the precedence for TitleFull should be:
-- title full, generated title, title body
data SecAttrs i = SecAttrs
  { secId :: Maybe Identifier
  , secTitleBody :: Maybe (Title i)
  , secTitleFull :: Maybe (Title i)
  , secNum :: Maybe ElemNumber
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

-- TODO: quite the hack here. We _need_ to set the title body to
-- Nothing. Otherwise identifiers in both would get double-counted.
instance Gathering note i i' => Gathering note (SecAttrs i) (SecAttrs i') where
  gathering (SecAttrs mi _ tf mn) = do
    tellLinkNumbered "" mi mn
    tf' <- gathering tf
    pure $ SecAttrs mi Nothing tf' mn

-- | A 'Section' is a major document division in the main matter. It
-- is the top-level section of articles.
data Section b i = Section
  { sectionAttrs :: SecAttrs i
  , sectionPageName :: Maybe PageName
  , sectionPreamble :: [b i]
  , sectionChildren :: [Subsection b i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

instance (Gathering note i i', Gathering note (b i) (b' i')) => Gathering note (Section b i) (Section b' i') where
  gathering (Section sa mpn sp sc) = do
    tellPageNode "section" mpn
    sa' <- gathering sa
    sp' <- gathering sp
    sc' <- gathering sc
    pure $ Section sa' mpn sp' sc'

data Subsection b i = Subsection
  { subsectionAttrs :: SecAttrs i
  , subsectionPageName :: Maybe PageName
  , subsectionContent :: [b i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

instance (Gathering note i i', Gathering note (b i) (b' i')) => Gathering note (Subsection b i) (Subsection b' i') where
  gathering (Subsection sa mpn sc) = do
    tellPageNode "section" mpn
    sa' <- gathering sa
    sc' <- gathering sc
    pure $ Subsection sa' mpn sc'

newtype Heading i = Heading
  { getHeading :: i
  } deriving (Eq, Ord, Show, Read, Generic, Functor)
    deriving anyclass (Numbering, Titling a)

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

-- TODO: deriving via?
-- TODO: necessary hack here, setting the titlebody to nothing. better to remove
-- the component entirely.
instance (Titling i (b i), Titling i i, FromTitleComponent i) => Titling i (Section b i) where
  titling (Section (SecAttrs mId mtbody mtfull mnum) mpage pre children) = do
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
        ((: []) . fromTitleNumber . elemNumberNum <$> mnum)
        (titleBody <$> mtbody')
    pure $ Section
      (SecAttrs mId Nothing (mtfull' <|> join mtigen <|> mtbody') mnum)
      mpage
      pre'
      children'

-- TODO: duplication
-- TODO: necessary hack here, setting the titlebody to nothing. better to remove
-- the component entirely.
instance (Titling i (b i), Titling i i, FromTitleComponent i) => Titling i (Subsection b i) where
  titling (Subsection (SecAttrs mId mtbody mtfull mnum) mpage c) = do
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
        ((: []) . fromTitleNumber . elemNumberNum <$> mnum)
        (titleBody <$> mtbody')
    pure $ Subsection
      (SecAttrs mId Nothing (mtfull' <|> join mtigen <|> mtbody') mnum)
      mpage
      c'

instance (Numbering (b i), Numbering i) => Numbering (Section b i) where
  numbering (Section (SecAttrs mId tbody tfull mnum) mpage pre child) =
    bracketNumbering (Just "section") $ \mnumgen -> do
      tbody' <- numbering tbody
      tfull' <- numbering tfull
      pre'   <- numbering pre
      child' <- numbering child
      pure $ Section (SecAttrs mId tbody' tfull' (mnum <|> mnumgen))
                     mpage
                     pre'
                     child'

instance (Numbering (b i), Numbering i) => Numbering (Subsection b i) where
  numbering (Subsection (SecAttrs mId tbody tfull mnum) mpage c) =
    bracketNumbering (Just "subsection") $ \mnumgen -> do
      tbody' <- numbering tbody
      tfull' <- numbering tfull
      c'     <- numbering c
      pure $ Subsection (SecAttrs mId tbody' tfull' (mnum <|> mnumgen)) mpage c'

instance (Referencing (f a) (g b), Referencing a b) => Referencing (Section f a) (Section g b)

instance (Referencing (f a) (g b), Referencing a b) => Referencing (Subsection f a) (Subsection g b)

instance Referencing a b => Referencing (SecAttrs a) (SecAttrs b)

-- TODO: add the section type as a class or data attribute

-- TODO: for untitled sections, perhaps conditionally add an anonymous
-- break? They would be necessary when we first render a sibling
-- untitled section. Some kind of state variable, I think.
instance (RH.Render (b i), RH.Render i) => RH.Render (Section b i) where
  render (Section (SecAttrs ml _ t _) _ pre c) = do
    t' <- traverse (RH.render . Heading) t
    let ident = identAttr <$> ml
    RH.bumpHeaderDepth $ do
      pre' <- RH.render pre
      c'   <- RH.render c
      let prewrap = Html.div Html.! HtmlA.class_ "sectionPreamble" $ pre'
          cwrap   = Html.div Html.! HtmlA.class_ "sectionContent" $ c'
      pure $ Html.section RH.?? ident $ fromMaybe mempty t' <> prewrap <> cwrap

-- TODO: duplication
instance (RH.Render (b i), RH.Render i) => RH.Render (Subsection b i) where
  render (Subsection (SecAttrs ml _ t _) _ c) = do
    t' <- traverse (RH.render . Heading) t
    let ident = identAttr <$> ml
    RH.bumpHeaderDepth $ do
      c' <- RH.render c
      let cwrap = Html.div Html.! HtmlA.class_ "sectionPreamble" $ c'
      pure $ Html.section RH.?? ident $ fromMaybe mempty t' <> cwrap

-- * Parsing

-- TODO: the one invocation gives the error "insufficient nodes",
-- which is unhelpful here. Should be fixed ("expecting exactly
-- one..."), perhaps with a "symbol" parser.
pDoc
  :: HasStr j
  => Scriba Node j
  -> ([j] -> Text)
  -> Scriba Node (b i)
  -> Scriba Node i
  -> Scriba Element (Doc b j i)
pDoc pMetInl stripMarkup pBlk pInl = whileMatchTy "scriba" $ do
  t <- meta $ attrs $ attr "type" $ content $ do
    consumeWhiteSpace
    t <- one simpleText
    consumeWhiteSpace
    zero
    pure t
  case t of
    "article" -> Doc () . DocArticle <$> pArticle pMetInl stripMarkup pBlk pInl
    _         -> throwError $ Msg $ "unknown document type: " <> t

-- TODO: more robust parsing here, including digitized vs
-- non-digitized types.
-- TODO: allow only a subset of the *Matter to be present to be parsed
-- with the explicit matter.
pArticle
  :: HasStr j
  => Scriba Node j
  -> ([j] -> Text)
  -> Scriba Node (b i)
  -> Scriba Node i
  -> Scriba Element (Article b j i)
pArticle pMetInl stripMarkup pBlk pInl = do
  dm <- meta $ attrs $ pDocAttrs pMetInl stripMarkup
  content $ pExplicitMatter dm <|> pBare dm
 where
  pExplicitMatter dm = do
    f <- one $ asNode $ pFrontMatter pBlk
    m <- one $ asNode $ pMainMatter pBlk pInl
    pure $ Article dm ArticleAttrs f m
  pBare dm = do
    b <- manyOf pBlk
    c <- remaining $ asNode $ pSection pBlk pInl
    pure $ Article dm ArticleAttrs [Introduction b] c

pFrontMatter :: Scriba Node (b i) -> Scriba Element [FrontMatter b i]
pFrontMatter pBlk =
  whileMatchTy "frontMatter" $ allContentOf $ asNode $ pFrontMatterSec pBlk

pFrontMatterSec :: Scriba Node (b i) -> Scriba Element (FrontMatter b i)
pFrontMatterSec pBlk = pIntro <|> pDedication <|> pForeword
 where
  pSectionlike t f = fmap f $ whileMatchTy t $ allContentOf pBlk
  pIntro      = pSectionlike "introduction" Introduction
  pDedication = pSectionlike "dedication" Dedication
  pForeword   = pSectionlike "foreword" Foreword

pMainMatter
  :: Scriba Node (b i) -> Scriba Node i -> Scriba Element [Section b i]
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
                  (NumberSource . T.concat <$> mnumber)

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
  pure $ Section sattr Nothing pre children

-- TODO: duplication?
pSubsection
  :: Scriba Node (b i) -> Scriba Node i -> Scriba Element (Subsection b i)
pSubsection pBlk pInl = whileMatchTy "subsection" $ do
  sattr <- meta $ attrs $ pSecAttrs pInl
  c     <- content $ manyOf pBlk
  pure $ Subsection sattr Nothing c
