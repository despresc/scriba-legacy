{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: review exports once the refactor is done

module Text.Scriba.Markup
  ( Doc(..)
  , SectionContent(..)
  , Section(..)
  , Block(..)
  , MixedBody(..)
  , Formal(..)
  , List(..)
  , Paragraph(..)
  , Inline(..)
  , Title(..)
  , DocAttrs(..)
  , NumberStyle(..)
  , TitlingConfig(..)
  , unzips
  , unzips3
  , FormalConfig(..)
  , runTemplate
  , TitleTemplateStyle(..)
  , SectionConfig(..)
  , parseDoc
  , prettyScribaError
  , Str(..)
  , Emph(..)
  , Quote(..)
  , InlineMath(..)
  , DisplayMath(..)
  , InlineCode(..)
  , PageMark(..)
  , TitlePart(..)
  , TitleComponent(..)
  , Varied(..)
  , VariedVar(..)
  , BlockCode(..)
  , Ref(..)
  , NumberConfig(..)
  , decorate
  )
where

import           Text.Scriba.Decorate.Common
import           Text.Scriba.Counters
import           Text.Scriba.Intermediate
import           Text.Scriba.Element
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling

import           Control.Monad                  ( join )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Merge.Strict         as M
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           GHC.Generics                   ( Generic )

{- TODO:

- Better errors everywhere (use source positions, for one thing)

- Clarify whitespace policy. Right now none of the parsers allow
  whitespace around elements when recognizing them. This is fine when
  the output comes from a parsed sml document, but maybe we don't want
  to rely on that. That also means that elements with unusual
  presentations might not be parsed correctly.

- Page mark (physPage) might need to be some kind of locator term, or
  at least some kind of parsed value (to support, e.g., linking to
  page images).

- related: formal blocks might be used for "discussion", "note", or
  "proof". These discussion-type blocks are often discussions of
  particular things (theorem-like blocks, other sections, other books
  or things in other books if we have linking). We could have some
  kind of "relates to #thing" property, but we would have to decide
  how that gets rendered - maybe we use a title template to have "(of
  Theorem 7.4)" appended to "Proof" in the title of a formal block?

- simple local imports (more or less parsed textual inclusion to split
  up a document into multiple files). Would probably need a "main" or
  "index" document.

- document linker. This can manifest in a few ways:

  - cross-document linking. Each volume of an encyclopedia or
    collected works could be digitized separately, then linked
    together into a big composite document. Would allow checked
    inter-document links (see discussion related to formal blocks)

  - composite documents. Sort of the above: combine multiple articles
    into a single custom journal issue, take a document and annotate
    it with your commentary, that sort of thing.

- fallbacks and conditional rendering for the various output formats?
  Say for internal links in non-hypertext formats. Though there we
  might have some automatic fallback options (e.g. if we link to a
  numbered thing we could put its type and number in parentheses after
  the link text).

- the parsers need to be aware of failure while consuming input! I
  don't think our current error discipline handles this very
  well. E.g. if we have a document with a # frontMatter then our
  parser ought to attempt to parse the document with an explicit
  matter structure. What it should _not_ do, and what it currently
  does, is fall back to parsing it with the implicit matter structure.

- for inline Code, we may want to do what some markdowns do and turn
  newlines into spaces

- a reasonable number of the `one text` usages could instead be
  `allContentOf ...` with a concat.

- facility for replacing typewriter apostrophe with right single
  quotation mark? Might just be a style guide thing, though.

- the Doc should probably have room for a plain text title in its
  attributes. Something to put in the HTML title, the database, and so
  on.

- Now that we have an "AsPara" presentation, we may want to parse
  things presented as paragraphs as paragraphs and not give them the
  "p" type in Intermediate. We should also let source presentation
  influence errors, so that, say, an unexpected syntactic paragraph
  would give the error "unexpected paragraph", not "unexpected element
  p".

- For the formal and section config, we might want different styles as
  in amsmath, perhaps custom ones too. What these entail could be
  configurable through css and the tex header.

- For lists, and numbered things generally, we should assume a default
  numbering style, so that references to numbered list items can still
  be rendered visually in HTML. Alternate styles could still be set,
  but this would have to be done in-document, and it would be the
  responsibility of the user (if not using whatever standalone
  rendering we support) to ensure that the CSS matches what the
  document assumes. We could still ship with CSS styles for the
  numbers, and simply put different classes on the rendered lists, of
  course.

- label/ref. Things that can be numbered should have reference
  prefixes, with upper/lower case and plural forms (for multiple
  collected references).

Unified element type configuration? Elements can have have

- identifiers
- numbers

-}

data Block a
  = Bformal !(Formal Block a)
  | Bcode !BlockCode
  | Bpar !(Paragraph a)
  | Blist !(List Block a)
  deriving (Eq, Ord, Show, Read, Generic, Functor, Numbering i)

deriving instance (FromTitleComponent i, Titling i i) => Titling i (Block i)
instance Referencing i (Inline a) (Inline b) => Referencing i (Block (Inline a)) (Block (Inline b))

-- TODO: rename Math to InlineMath?
-- TODO: Number is [Inline] because I'm lazy with runVaried. It should
-- probably be Text, and might want to record the Int that produced
-- that number, or maybe be a list of number parts? We might want to
-- call it something different. Tag, or something.
-- TODO: Should probably make this type more modular. Would allow us
-- greater control over the usage of inlines, allow greater
-- modularization of functions
data Inline a
  = Istr !Str
  | Iemph !(Emph (Inline a))
  | Iquote !(Quote (Inline a))
  | IinlineMath !InlineMath
  | IdisplayMath !DisplayMath
  | Icode !InlineCode
  | IpageMark !PageMark
  | Iref !(Ref (Inline a))
  | ItitleComponent !(TitleComponent (Inline a))
  | Icontrol !a
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering i, Titling i)

-- TODO: could make this more flexible with a "without" class

-- TODO: figure something out here.
-- I have a (SourceRef -> RefM i (Ref i)). I can promote that, right
-- now, to an InlineControl -> RefM i (Ref i)
-- Then I can promote that to an (Inline InlineControl -> RefM i
-- (Inline i) The issue is that to be aware of that, the deriving
-- mechanism needs to be aware of the monadic nature of Inline, and so
-- be able to take what is in effect an (a -> Inline b) and turn that
-- into an (Inline a -> Inline b). So essentially I need another class
-- to make this nice
instance Referencing (Inline Void) (Inline InlineControl) (Inline Void) where
  referencing (Istr            x) = Istr <$> referencing x
  referencing (Iemph           x) = Iemph <$> referencing x
  referencing (Iquote          x) = Iquote <$> referencing x
  referencing (IinlineMath     x) = IinlineMath <$> referencing x
  referencing (IdisplayMath    x) = IdisplayMath <$> referencing x
  referencing (Icode           x) = Icode <$> referencing x
  referencing (IpageMark       x) = IpageMark <$> referencing x
  referencing (Iref            x) = Iref <$> referencing x
  referencing (ItitleComponent x) = ItitleComponent <$> referencing x
  referencing (Icontrol        x) = referencing x

instance Referencing (Inline b) InlineControl (Inline b) where
  referencing (IcRef sr) = Iref <$> resolveRef sr

data InlineControl
  = IcRef !SourceRef
  deriving (Eq, Ord, Show, Read, Generic, Numbering i, Titling i)

instance FromTitleComponent (Inline a) where
  fromTitleComponent = ItitleComponent
  fromTitleNumber    = Istr . Str

-- | Strip out the markup in a sequence of inlines, leaving only the
-- plain text. Very lossy, naturally. This doesn't add any textual
-- markers around code, math, or quotations, or things like that, so
-- beware.
stripMarkup :: (a -> [Text]) -> [Inline a] -> Text
stripMarkup f = T.intercalate " " . T.words . T.concat . concatMap inlineToText
 where
  inlineToText (Istr            i ) = strToText i
  inlineToText (Iemph           is) = emphToText inlineToText is
  inlineToText (Iquote          is) = quoteToText inlineToText is
  inlineToText (IinlineMath     t ) = inlineMathToText t
  inlineToText (IdisplayMath    d ) = displayMathToText d
  inlineToText (Icode           t ) = inlineCodeToText t
  inlineToText (IpageMark       t ) = pageMarkToText t
  inlineToText (Iref            t ) = refToText inlineToText t
  inlineToText (ItitleComponent t ) = titleComponentToText inlineToText t
  inlineToText (Icontrol        a ) = f a

-- * Element parsers

-- ** Document attribute parsing

-- Content with variables and text in it, for use in formal
-- config. 
-- TODO: Should probably have the internal representation be
-- flexible enough to accommodate variables and things.

-- TODO: Some sort of conditional behaviour may become necessary in
-- templates. There may be examples where separator styling needs to
-- vary based on the presence of certain elements.
-- TODO: need a default [Varied] template.
data Varied
  = VariedStr Text
  | VariedSpace
  | VariedVar Text VariedVar Text
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: restrict the appearance of particular variables? Right now we
-- use the same pVaried for formal blocks and sections.
data VariedVar
  = VariedNote
  | VariedTitleBody
  | VariedPrefix
  | VariedNumber
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: write this. Will need to have options for how they are
-- numbered, and how the title and conclusion are generated.
-- TODO: should probably have "whileParsingAttr"
-- TODO: put in checks on the possible {type} of formal blocks?
-- TODO: better errors in some of the subparsers

-- TODO: not entirely sure what to do here. I _think_ I want the title
-- template to have variables and known blocks, but that will require
-- my syntax tree to be a little more flexible.
-- For now we just have variable parsing implemented here.

-- TODO: I think we want to be able to give the components of the
-- generated title different styles. E.g. have an
-- optionalParenthetical style, or something like that, for the
-- titleNote, so that CSS doesn't have to be touched too much.

-- TODO: think about the title sep more carefully.  
-- TODO: need to think about NumberStyle. Later, we just M.mapMaybe id
-- what we get from unzip3, but perhaps this isn't wise?

-- TODO: should have a NumberingConfig, TitlingConfig, etc., I think,
-- keyed by the type of the element. Then we can operate on numbered
-- things somewhat uniformly internally.

-- TODO: reconside the name of "title" for the title
-- template/configuration. Also reconsider the other configuration
-- names. They should have the same name as the corresponding title
-- attributes.
-- TODO: move this (and the other attribute parsers) to Titling.
-- TODO: Should the number config be a Maybe (and be in with
-- ContainerRelation?)
pFormalConfig
  :: Scriba
       Element
       ( Map
           Text
           ( FormalConfig (Inline a)
           , Maybe ContainerRelation
           , Maybe (NumberConfig (Inline a))
           )
       )
pFormalConfig = meta $ attrs $ allAttrsOf pFormalSpec
 where
  pFormalSpec = do
    t <-
      ty $ inspect >>= maybe (throwError $ Msg "a block type is required") pure
    whileParsingElem ("formal block " <> t <> " config") $ meta $ attrs $ do
      titleTemplate <- attrMaybe "title" $ meta $ attrs
        (pTitleTemplate FormalTemplate)
      titleSep          <- attrMaybe "titleSep" $ allContentOf pInlineCore
      numberingConf     <- attrMaybe "numbering" $ pCounterDepends
      (refSep, refPref) <- fmap unzips $ attrMaybe "ref" $ meta $ attrs $ do
        s <- attrMaybe "sep" $ allContentOf pInlineCore
        p <- attrMaybe "prefix" $ allContentOf pInlineCore
        pure (s, p)
      concl <- attrMaybe "conclusion" $ allContentOf pInlineCore
      let (counterDepends, nc) = unzips $ do
            (containerRel, ns) <- numberingConf
            pure (containerRel, NumberConfig ns (join refPref) (join refSep))
      pure $ (FormalConfig titleTemplate titleSep concl, counterDepends, nc)

-- TODO: more exotic orderings. Perhaps make prefix/numberFirst
-- exclusive as well. Also options for suppressing particular
-- components, if, say, you wanted something numbered but not have it
-- actually appear in the title.
-- TODO: title separator customization?
pTitleTemplate :: TitleTemplateStyle -> Scriba Attrs (TitleTemplate (Inline a))
pTitleTemplate t = do
  pref        <- attrDef "prefix" emptySurround $ pSurround
  tnote       <- attrDef tname emptySurround $ pSurround
  tnum        <- attrDef "n" emptySurround $ pSurround
  prefixFirst <- attrMaybe "prefixFirst" $ pure True
  numberFirst <- attrMaybe "numberFirst" $ pure False
  pure $ TitleTemplate pref
                       tnum
                       tnote
                       [Istr $ Str " "]
                       (fromMaybe True $ prefixFirst <|> numberFirst)
 where
  tname = case t of
    FormalTemplate  -> "note"
    SectionTemplate -> "titleBody"

-- TODO: can't distinguish between present-but-empty content, and
-- absent content. Seems okay right now.
pSurround :: Scriba Element (Surround (Inline a))
pSurround = do
  (b, a) <- meta $ attrs $ do
    b <- mattr "before" $ allContentOf pInlineCore
    a <- mattr "after" $ allContentOf pInlineCore
    pure (b, a)
  c <- allContentOf $ pInlineCore
  let c' = case c of
        [] -> Nothing
        x  -> Just x
  pure $ Surround b c' a

-- TODO: reduce duplication with pFormalConfig
pSectionConfig
  :: Scriba
       Element
       ( Map
           Text
           ( SectionConfig (Inline a)
           , Maybe ContainerRelation
           , Maybe (NumberConfig (Inline a))
           )
       )
pSectionConfig = meta $ attrs $ allAttrsOf pSectionSpec
 where
  pSectionSpec = do
    t <-
      ty
      $   inspect
      >>= maybe (throwError $ Msg "a section type is required") pure
    whileParsingElem ("section " <> t <> " config") $ meta $ attrs $ do
      titleTemplate <- attrMaybe "title" $ meta $ attrs
        (pTitleTemplate SectionTemplate)
      numberingConf     <- attrMaybe "numbering" $ pCounterDepends
      (refSep, refPref) <- fmap unzips $ attrMaybe "ref" $ meta $ attrs $ do
        s <- attrMaybe "sep" $ allContentOf pInlineCore
        p <- attrMaybe "prefix" $ allContentOf pInlineCore
        pure (s, p)
      let (counterDepends, nc) = unzips $ do
            (containerRel, ns) <- numberingConf
            pure (containerRel, NumberConfig ns (join refPref) (join refSep))
      pure $ (SectionConfig titleTemplate, counterDepends, nc)

-- TODO: enforce option exclusivity? Could do that by running all
-- parsers (which should also return expectations), then throw an
-- error if at least two are Right.
-- TODO: need some pOneArg thing, clearly
pCounterDepends :: Scriba Element (ContainerRelation, NumberStyle)
pCounterDepends = meta $ attrs $ do
  cr <- pAbsCounter <|> pRelCounter <|> pShare
  ns <- pNumberStyle
  pure (cr, ns)
 where
  pAbsCounter = attr "absolute" $ pure $ Relative []
  pRelCounter =
    fmap (Relative . map ContainerName) $ attr "relative" $ meta $ allArgsOf
      simpleText
  pShare = attr "share" $ meta $ do
    ts <- allArgsOf simpleText
    case ts of
      [t] -> pure $ Share $ ContainerName t
      _   -> throwError $ Msg "expecting exactly one counter"
  pNumberStyle = attrDef "style" Decimal $ pure Decimal

-- ** Block Parsing

pBlock :: Scriba Node (Inline a) -> Scriba Node (Block (Inline a))
pBlock pInl =
  asNode
    $   Bformal
    <$> pFormal (pMixedBody pInl) (pInlineBody pInl)
    <|> Bpar
    <$> pParagraph (pInlineBody pInl)
    <|> Bcode
    <$> pBlockCode
    <|> Blist
    <$> pList (pMixedBody pInl)

pBlockBody :: Scriba Node (Inline a) -> Scriba [Node] [Block (Inline a)]
pBlockBody pInl = remaining $ pBlock pInl

pInlineBody :: Scriba Node (Inline a) -> Scriba [Node] [Inline a]
pInlineBody = remaining

pMixedBody
  :: Scriba Node (Inline a) -> Scriba [Node] (MixedBody Block (Inline a))
pMixedBody pInl =
  MixedBlock <$> (pBlockBody pInl) <|> MixedInline <$> (pInlineBody pInl)

-- ** Inline parsing

-- pInline is synonymous with pParInline
pInline :: Scriba Node (Inline InlineControl)
pInline =
  asNode
      (   Iemph
      <$> pEmph pInline
      <|> Iquote
      <$> pQuote pInline
      <|> IpageMark
      <$> pPageMark
      <|> IinlineMath
      <$> pMath
      <|> IdisplayMath
      <$> pFormula
      <|> IdisplayMath
      <$> pGathered
      <|> Icode
      <$> pCode
      <|> Icontrol
      <$> pControl
      )
    <|> Istr
    <$> pText

-- TODO: duplication
pInlineCore :: Scriba Node (Inline a)
pInlineCore =
  asNode
      (   Iemph
      <$> pEmph pInlineCore
      <|> Iquote
      <$> pQuote pInlineCore
      <|> IpageMark
      <$> pPageMark
      <|> IinlineMath
      <$> pMath
      <|> IdisplayMath
      <$> pFormula
      <|> IdisplayMath
      <$> pGathered
      <|> Icode
      <$> pCode
      )
    <|> Istr
    <$> pText

-- ** Section parsing

-- For now, all things presented as sections become sections.

-- TODO: do the expectations actually work out here?
-- TODO: a top level title parser?
-- TODO: reduce duplication with pFormalConfig
pSection :: Scriba Node (Inline a) -> Scriba Element (Section Block (Inline a))
pSection pInl = do
  mty <- (matchTy "section" $> Just "section") <|> presentedAsSection
  whileParsingElem (fromMaybe "section of unknown type" mty) $ do
    (mId, mtitle, mfullTitle, mnumber) <- meta $ attrs $ do
      mId        <- attrMaybe "id" $ content pIdent
      mtitle     <- attrMaybe "title" $ allContentOf pInl
      mfullTitle <- attrMaybe "fullTitle" $ allContentOf pInl
      mnumber    <- attrMaybe "n" $ allContentOf simpleText
      pure (mId, mtitle, mfullTitle, mnumber)
    c <- content $ pSectionContent pInl
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

-- implicitly parses the whole block content

-- The errors are better, but if we, e.g., have an unrecognized block,
-- then the manyOf fails, and we get an "expecting one of: section",
-- intead of also listing the blocks that we could have. Perhaps we
-- should instead have a more restrictive parser type?
-- (optparse-applicative being an example of that sort of thing) We'd
-- want better expectation setting and propagation, certainly. Maybe
-- also some conveniences like traversing a parser to get a text
-- description of the node structure that it recognizes.
pSectionContent
  :: Scriba Node (Inline a) -> Scriba [Node] (SectionContent Block (Inline a))
pSectionContent pInl = do
  pre  <- manyOf $ pBlock pInl
  subs <- remaining $ asNode (pSection pInl)
  pure $ SectionContent pre subs

pControl :: Scriba Element InlineControl
pControl = IcRef <$> pSourceRef

-- ** Document parsing

-- TODO: have a pSectionNamed :: Text -> Scriba Element Section to
-- deal with special sections, like the matter?
-- TODO: deal with this allContent invocation (and any other
-- troublesome ones).
-- TODO: the pBare dm and pExplicitMatter dm thing is a bit bad.
-- TODO: Enforce non-empty title for a doc? Or perhaps just warn on one.
-- TODO: For error purposes it might be better if the meta is in a
-- whileParsing "document meta", and the body is in a whileParsing
-- "document body", but this is somewhat stylistic.
-- TODO: error in the container relations compiling
-- TODO: document section config takes precedence over formal block
-- config re: counters, in particular that there is no namespacing
-- going on.
-- TODO: add configuration for elemrel
pDoc :: Scriba Element (Doc Block (Inline a) (Inline InlineControl))
pDoc = do
  matchTy "scriba"
  whileParsingElem "scriba" $ do
    dm <- meta $ attrs $ do
      t       <- mattr "title" $ allContentOf pInlineCore
      tplain <- attrMaybe "plainTitle" $ fmap T.concat $ allContentOf simpleText
      fconfig <- mattr "formalBlocks" $ pFormalConfig
      sconfig <- mattr "sections" $ pSectionConfig
      -- TODO: clean up
      let (fconf, frelRaw, fnstyleRaw) = unzips3 fconfig
          mcrel (x, y) = (,) (ContainerName x) <$> y
          (sconf, srelRaw, snstyleRaw) = unzips3 sconfig
      (elemrel, crel) <-
        case
          compileContainerRelations
            (mapMaybe mcrel $ M.toList srelRaw <> M.toList frelRaw)
        of
          Left  e -> throwError $ Msg e
          Right a -> pure a
      let
        mergeRel =
          M.merge M.dropMissing M.dropMissing $ M.zipWithMatched $ const (,)
        toCNKey  = M.mapKeysMonotonic ContainerName
        elemrel' = mergeRel
          elemrel
          (M.mapMaybe id $ toCNKey snstyleRaw <> toCNKey fnstyleRaw) -- M.map (\x -> (x, NumberConfig Decimal Nothing)) elemrel
      pure $ DocAttrs (Title t)
                      (fromMaybe (stripMarkup (const []) t) tplain)
                      (TitlingConfig fconf sconf)
                      elemrel'
                      crel
--                      (snstyle <> fnstyle)
    content $ pExplicitMatter dm <|> pBare dm
 where
  pMatter t = asNode $ do
    matchTy t
    whileParsingElem t $ content $ pSectionContent pInline
  pExplicitMatter dm = do
    f <- one $ pMatter "frontMatter"
    m <- one $ pMatter "mainMatter"
    b <- one $ pMatter "endMatter"
    zero
    pure $ Doc dm f m b
  pBare dm = do
    c <- pSectionContent pInline
    pure $ Doc dm emptySectionContent c emptySectionContent

-- * Running parsers

parseDoc
  :: Node -> Either ScribaError (Doc Block (Inline a) (Inline InlineControl))
parseDoc = fmap snd . runScriba (asNode pDoc)

-- * Decorating the document

-- TODO: add in configuration for prefixes and other things
defaultNumberState :: DocAttrs i -> NumberState i
defaultNumberState da = NumberState initCounters
                                    []
                                    (docCounterRel da)
                                    (docElemCounterRel da)
                                    mempty
  where initCounters = docCounterRel da $> 1


getRefEnv :: NumberData i -> RefData i
getRefEnv (NumberData d) = RefData $ M.fromList $ go <$> d
  where go (NumberDatum i cn nc num) = (i, (cn, nc, num))

-- TODO: don't discard the numbering information.
runNumDoc
  :: Numbering j a
  => Doc Block j (Inline a)
  -> (NumberData j, Doc Block j (Inline a))
runNumDoc d@(Doc da _ _ _) =
  flip runNumberM (defaultNumberState da) $ numbering d

runTitleDoc
  :: forall a
   . Titling (Inline a) a
  => Doc Block (Inline Void) (Inline a)
  -> Doc Block (Inline Void) (Inline a)
runTitleDoc d@(Doc da _ _ _) =
  flip runTitleM (fmap (traverseInline absurd) $ docTitlingConfig da)
    $ (titling :: Doc Block (Inline Void) (Inline a)
        -> TitleM (Inline a) (Doc Block (Inline Void) (Inline a))
      )
        d

runRefDoc
  :: Referencing (Inline Void) (Inline a) (Inline b)
  => RefData (Inline Void)
  -> Doc Block j (Inline a)
  -> Either DecorateError (Doc Block j (Inline b))
runRefDoc rd d = runRefM (referencing d) rd

-- TODO: obviously have this be automatic. I suppose Inline is a
-- monad.
traverseInline :: (a -> Inline b) -> Inline a -> Inline b
traverseInline f (Icontrol a) = f a
traverseInline f (Iemph    e) = Iemph $ fmap (traverseInline f) e
traverseInline f (Iquote   e) = Iquote $ fmap (traverseInline f) e
traverseInline f (Iref     e) = Iref $ fmap (traverseInline f) e
traverseInline f (ItitleComponent e) =
  ItitleComponent $ fmap (traverseInline f) e
traverseInline _ (Istr         s) = Istr s
traverseInline _ (IinlineMath  s) = IinlineMath s
traverseInline _ (IdisplayMath s) = IdisplayMath s
traverseInline _ (Icode        s) = Icode s
traverseInline _ (IpageMark    s) = IpageMark s


-- TODO: may need errors, a state for numbering, environment for
-- titling. Or perhaps pipelined for modularity, with a shared error
-- type?
-- TODO: do something with the control elements, obviously.
-- TODO: _very_ bad hack here, with adjustEnv. Might want to add a
-- parameter to Doc.
decorate
  :: Doc Block (Inline Void) (Inline InlineControl)
  -> Either DecorateError (Doc Block (Inline Void) (Inline Void))
decorate d =
  let (numdat, nd) = runNumDoc d
      td           = runTitleDoc nd
      erd          = runRefDoc (adjustEnv $ getRefEnv numdat) td
  in  erd
 where
  adjustEnv (RefData m) = RefData $ (\(x, y, z) -> (x, adjustNC y, z)) <$> m
  adjustNC (NumberConfig ns m p) =
    NumberConfig ns (map adjustInls <$> m) (map adjustInls <$> p)
  adjustInls = traverseInline $ const $ Istr $ Str ""

