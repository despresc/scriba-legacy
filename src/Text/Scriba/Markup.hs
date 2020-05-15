{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , OlistItem(..)
  , Paragraph(..)
  , Inline(..)
  , Title(..)
  , Heading(..)
  , DocAttrs(..)
  , LocalNumberStyle(..)
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
  , BlockCode(..)
  , Ref(..)
  , NumberConfig(..)
  , UsedNumberConfig(..)
  , Identifier(..)
  , ContainerName(..)
  , MathItem(..)
  , decorate
  , writeStandalone
  , MathJaxConfig(..)
  , StandaloneConfig(..)
  )
where

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Monad                  ( join
                                                , unless
                                                )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Aeson                     ( ToJSON(..) )
import qualified Data.Aeson                    as Aeson
import           Data.Char                      ( isAlpha )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Merge.Strict         as M
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

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

- We currently have the noNum attribute for suppressing the numbering
  of displayed equations. It might be better to have boolean syntax
  for these things. !num and ~num for true and false? unsure.

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
  | Iname !(Name (Inline a))
  | IworkTitle !(WorkTitle (Inline a))
  | Iregularize !(Regularize (Inline a))
  | Icite !(Cite (Inline a))
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
-- (Inline i)) The issue is that to be aware of that, the deriving
-- mechanism needs to be aware of the monadic nature of Inline, and so
-- be able to take what is in effect an (a -> Inline b) and turn that
-- into an (Inline a -> Inline b). So essentially I need another class
-- to make this nice
instance Referencing (Inline Void) (Inline InlineControl) (Inline Void) where
  referencing (Istr            x) = Istr <$> referencing x
  referencing (Iemph           x) = Iemph <$> referencing x
  referencing (Iquote          x) = Iquote <$> referencing x
  referencing (Iname           x) = Iname <$> referencing x
  referencing (IworkTitle      x) = IworkTitle <$> referencing x
  referencing (Iregularize     x) = Iregularize <$> referencing x
  referencing (Icite           x) = Icite <$> referencing x
  referencing (IinlineMath     x) = IinlineMath <$> referencing x
  referencing (IdisplayMath    x) = IdisplayMath <$> referencing x
  referencing (Icode           x) = Icode <$> referencing x
  referencing (IpageMark       x) = IpageMark <$> referencing x
  referencing (Iref            x) = Iref <$> referencing x
  referencing (ItitleComponent x) = ItitleComponent <$> referencing x
  referencing (Icontrol        x) = referencing x

instance Referencing (Inline b) InlineControl (Inline b) where
  referencing (IcRef sr) = Iref <$> resolveRef sr

newtype InlineControl
  = IcRef SourceRef
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Numbering i, Titling i)

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
  inlineToText (Iname           is) = nameToText inlineToText is
  inlineToText (IworkTitle      is) = workTitleToText inlineToText is
  inlineToText (Iregularize     is) = regularizeToText inlineToText is
  inlineToText (Icite           is) = citeToText inlineToText is
  inlineToText (IinlineMath     t ) = inlineMathToText t
  inlineToText (IdisplayMath    d ) = displayMathToText d
  inlineToText (Icode           t ) = inlineCodeToText t
  inlineToText (IpageMark       t ) = pageMarkToText t
  inlineToText (Iref            t ) = refToText inlineToText t
  inlineToText (ItitleComponent t ) = titleComponentToText inlineToText t
  inlineToText (Icontrol        a ) = f a

-- * Element parsers

-- ** Document attribute parsing

-- TODO: put in checks on the possible {type} of formal blocks?

-- TODO: think about the title sep more carefully.

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
      titleSep <- attrMaybe "titleSep" $ allContentOf pInlineCore
      (cr, nc) <- pNumberRef
      concl    <- attrMaybe "conclusion" $ allContentOf pInlineCore
      pure
        ( FormalConfig titleTemplate titleSep concl
        , cr
        , nc <*> pure FilterByCounterDep
        )

defaultListConfig :: NumberConfig (Inline a)
defaultListConfig = NumberConfig
  ( NumberStyle (FilterByContainer "item:olist") Nothing
  $ DepthStyle [Decimal, LowerAlpha, LowerRoman, Decimal]
  )
  (Just [Istr $ Str "item"])
  (Just [Istr $ Str " "])

-- TODO: add actual config
pListConfig :: Scriba Element (NumberConfig (Inline a))
pListConfig =
  meta $ attrs $ attrDef "olist" defaultListConfig $ pure defaultListConfig

-- TODO: more exotic orderings. Perhaps make prefix/numberFirst
-- exclusive as well. Also options for suppressing particular
-- components, if, say, you wanted something numbered but not have it
-- actually appear in the title.
-- TODO: title separator customization?
pTitleTemplate :: TitleTemplateStyle -> Scriba Attrs (TitleTemplate (Inline a))
pTitleTemplate t = do
  pref        <- attrDef "prefix" emptySurround pSurround
  tnote       <- attrDef tname emptySurround pSurround
  tnum        <- attrDef "n" emptySurround pSurround
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
  c <- allContentOf pInlineCore
  let c' = case c of
        [] -> Nothing
        x  -> Just x
  pure $ Surround b c' a

-- TODO: add in full NumberStyle parsing in some way. It might be good
-- to have the config be dependent on the carrier of the container
-- type, so that list item numbering would always (or by default) be
-- by depth. Could simply pass in a number style parser.  TODO: I need
-- to recreate this for list numbering parsing anyway, so we probably
-- don't need to return a Maybe function here. Just have the filter be
-- by relatedness.
pNumberRef
  :: Scriba
       Attrs
       ( Maybe ContainerRelation
       , Maybe (ContainerPathFilter -> NumberConfig (Inline a))
       )
pNumberRef = do
  numberingConf     <- attrMaybe "numbering" pCounterDepends
  (refSep, refPref) <- fmap unzips $ attrMaybe "ref" $ meta $ attrs $ do
    s <- attrMaybe "sep" $ allContentOf pInlineCore
    p <- attrMaybe "prefix" $ allContentOf pInlineCore
    pure (s, p)
  let
    (counterDepends, nc) = unzips $ do
      (containerRel, ns) <- numberingConf
      pure
        ( containerRel
        , \fm ->
          NumberConfig (NumberStyle fm Nothing ns) (join refPref) (join refSep)
        )
  pure (counterDepends, nc)

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
      (cr, nc) <- pNumberRef
      pure (SectionConfig titleTemplate, cr, nc <*> pure FilterByCounterDep)

-- TODO: enforce option exclusivity? Could do that by running all
-- parsers (which should also return expectations), then throw an
-- error if at least two are Right.
-- TODO: need some pOneArg thing, clearly

-- TODO: change pNumberStyle back to a single local number style. Then
-- create a new parser for pListConfig, since this one can't be used
-- for it (too dissimilar).
pCounterDepends :: Scriba Element (ContainerRelation, LocalStyle)
pCounterDepends = meta $ attrs $ do
  cr <-
    label "absolute" pAbsCounter
    <|> label "relative" pRelCounter
    <|> label "share"    pShare
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
  pNumberStyle = attrDef "style" (AbsoluteStyle Decimal) $ do
    as <- meta $ allArgsOf pLocalNumberStyle
    case as of
      []  -> throwError $ Msg "expecting one style, or multiple styles"
      [t] -> pure $ AbsoluteStyle t
      ts  -> pure $ DepthStyle ts
  pLocalNumberStyle = do
    (sp, t) <- text
    whileParsing (Just sp) "local number style" $ case t of
      "decimal" -> pure Decimal
      _         -> throwError $ Msg "unknown number style"

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
  MixedBlock <$> pBlockBody pInl <|> MixedInline <$> pInlineBody pInl

-- ** Inline parsing

pInline :: Scriba Node (Inline InlineControl)
pInline =
  asNode
      (   Iemph
      <$> pEmph pInline
      <|> Iquote
      <$> pQuote pInline
      <|> Iname
      <$> pName pInline
      <|> IworkTitle
      <$> pWorkTitle pInline
      <|> Iregularize
      <$> pRegularize pInline
      <|> Icite
      <$> pCite pInline
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
      <|> Iname
      <$> pName pInlineCore
      <|> IworkTitle
      <$> pWorkTitle pInlineCore
      <|> Iregularize
      <$> pRegularize pInlineCore
      <|> Icite
      <$> pCite pInlineCore
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

-- TODO: Validation?
pMathMacros :: Scriba Attrs (Map Text (Int, Text))
pMathMacros = allAttrsOf pMathMacro
 where
  pMathMacro = do
    t <-
      ty
      $   inspect
      >>= maybe (throwError $ Msg "unexpected nameless math macro") pure
    unless (T.all isAlpha t)
      $  throwError
      $  Msg
      $  "math macro name \""
      <> t
      <> "\" should be entirely alphabetic characters"
    n <- meta $ attrs $ attrDef "args" 0 $ do
      nr <- allContentOf simpleText
      case safeRead (T.concat nr) of
        Just n | n >= 0 -> pure n
        _               -> throwError $ Msg "expected positive integer"
    c <- T.concat <$> allContentOf simpleText
    pure (n, c)



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
-- TODO: better math numbering support. The elemrel thing is particularly bad.
-- TODO: the explicit matter parsing is not correct - you should need
-- to specify only a subset of the *Matter
pDoc :: Scriba Element (Doc Block (Inline a) (Inline InlineControl))
pDoc = do
  matchTy "scriba"
  whileParsingElem "scriba" $ do
    dm <- meta $ attrs $ do
      t       <- mattr "title" $ allContentOf pInlineCore
      mmacros <- mattr "mathMacros" $ meta $ attrs pMathMacros
      tplain  <- attrMaybe "plainTitle" $ T.concat <$> allContentOf simpleText
      fconfig <- mattr "formalBlocks" pFormalConfig
      sconfig <- mattr "sections" pSectionConfig
      lconfig <- attrDef "lists" defaultListConfig pListConfig
      let lCrel = ("item:olist", Just $ Relative [])
      (dmathCrel, dmathNumConf) <-
        fmap unzips $ attrMaybe "formula" $ meta $ attrs pNumberRef
      let dmathrelRaw = [("formula", join dmathCrel)]
      -- TODO: clean up
      let (fconf, frelRaw, fnstyleRaw) = unzips3 fconfig
          mkName (x, y) = (,) (ContainerName x) <$> y
          (sconf, srelRaw, snstyleRaw) = unzips3 sconfig
      (elemrel, crel) <-
        case
          compileContainerRelations
            (  mapMaybe mkName
            $  dmathrelRaw
            <> [lCrel]
            <> M.toList srelRaw
            <> M.toList frelRaw
            )
        of
          Left  e -> throwError $ Msg e
          Right a -> pure a
      let
        mergeRel =
          M.merge M.dropMissing M.dropMissing $ M.zipWithMatched $ const (,)
        toCNKey = M.mapKeysMonotonic ContainerName
        formulaStyle =
          M.singleton "formula" $ join dmathNumConf <*> pure FilterByCounterDep
        listStyle = M.singleton "item:olist" (Just lconfig)
        mergedStyles =
          M.mapMaybe id
            $  listStyle
            <> formulaStyle
            <> toCNKey snstyleRaw
            <> toCNKey fnstyleRaw
        elemrel' = mergeRel elemrel mergedStyles
      pure $ DocAttrs (Title t)
                      (fromMaybe (stripMarkup (const []) t) tplain)
                      (TitlingConfig fconf sconf)
                      elemrel'
                      crel
                      mmacros
    content $ pExplicitMatter dm <|> pBare dm
 where
  pMatter t = asNode $ do
    matchTy t
    whileParsingElem t $ content $ pSectionContent pInline
  pExplicitMatter dm = do
    f <- one $ pMatter "frontMatter"
    m <- one $ pMatter "mainMatter"
    b <- one $ pMatter "backMatter"
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
-- TODO: might want to forbid, or have special configuration for, the
-- "item" counter.
defaultNumberState :: DocAttrs (Inline i) -> NumberState (Inline i)
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
  :: Numbering (Inline j) a
  => Doc Block (Inline j) (Inline a)
  -> Either
       DecorateError
       (NumberData (Inline j), Doc Block (Inline j) (Inline a))
runNumDoc d@(Doc da _ _ _) =
  flip runNumberM (defaultNumberState da) $ numbering d

-- TODO: type applications for titling?
runTitleDoc
  :: forall a
   . Titling (Inline a) a
  => Doc Block (Inline Void) (Inline a)
  -> Doc Block (Inline Void) (Inline a)
runTitleDoc d@(Doc da _ _ _) =
  flip runTitleM (traverseInline absurd <$> docTitlingConfig da)
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
traverseInline f (Icontrol    a) = f a
traverseInline f (Iemph       e) = Iemph $ fmap (traverseInline f) e
traverseInline f (Iquote      e) = Iquote $ fmap (traverseInline f) e
traverseInline f (Iname       e) = Iname $ fmap (traverseInline f) e
traverseInline f (IworkTitle  e) = IworkTitle $ fmap (traverseInline f) e
traverseInline f (Iregularize e) = Iregularize $ fmap (traverseInline f) e
traverseInline f (Icite       e) = Icite $ fmap (traverseInline f) e
traverseInline f (Iref        e) = Iref $ fmap (traverseInline f) e
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
decorate d = do
  (numdat, nd) <- runNumDoc d
  let td = runTitleDoc nd
  runRefDoc (getRefEnv numdat) td

-- * Rendering

-- TODO: could have a generic deriving thing here, I suppose. Would
-- want it to be output-agnostic, if possible.

instance RH.Render i => RH.Render (Block i) where
  render (Bformal fb) = RH.render fb
  render (Bcode   t ) = RH.render t
  render (Bpar    p ) = RH.render p
  render (Blist   b ) = RH.render b

instance RH.Render a => RH.Render (Inline a) where
  render (Istr            s) = RH.render s
  render (Iemph           s) = RH.render s
  render (Iquote          s) = RH.render s
  render (Iname           s) = RH.render s
  render (IworkTitle      s) = RH.render s
  render (Iregularize     s) = RH.render s
  render (Icite           s) = RH.render s
  render (IinlineMath     s) = RH.render s
  render (IdisplayMath    s) = RH.render s
  render (Icode           s) = RH.render s
  render (IpageMark       s) = RH.render s
  render (ItitleComponent s) = RH.render s
  render (Iref            s) = RH.render s
  render (Icontrol        s) = RH.render s

-- TODO: move this elsewhere?
newtype MathJaxConfig = MathJaxConfig
  { mjMacros :: Map Text (Int, Text)
  }

instance ToJSON MathJaxConfig where
  toJSON (MathJaxConfig macros) = Aeson.object
    ["tex" Aeson..= Aeson.object ["macros" Aeson..= jmacros]]
   where
    jmacros = toJSON $ M.map renderMacro macros
    renderMacro (n, t) | n == 0    = toJSON t
                       | otherwise = toJSON [toJSON t, toJSON n]

renderMathJaxConfig :: DocAttrs i -> TL.Text
renderMathJaxConfig dm =
  TLE.decodeUtf8 $ "MathJax=" <> Aeson.encode (toJSON mjc) <> ";"
  where mjc = MathJaxConfig $ docMathMacros dm

newtype StandaloneConfig = StandaloneConfig
  { standaloneCSS :: FilePath
  }

-- TODO: should probably remove this
-- TODO: for standalone rendering we should probably put the title of
-- the document in the header.
-- TODO: add configurability, especially re: the math.
-- TODO: have a header include option for documents. Hard-coding a
-- style path for the manual is obviously poor.
-- TODO: Have StandaloneConfig instead be (StandaloneConfig a) and
-- make it an HTML instance? If we are keeping it, of course.
renderStandalone
  :: (RH.Render (b i), RH.Render i, RH.Render j)
  => StandaloneConfig
  -> Doc b j i
  -> RH.RenderM Html.Html
renderStandalone (StandaloneConfig csspath) d@(Doc dm _ _ _) = do
  d' <- RH.render d
  let tplain = docPlainTitle dm
  pure $ Html.docTypeHtml $ do
    Html.head $ do
      Html.meta Html.! HtmlA.charset "UTF-8"
      Html.title $ Html.toHtml tplain
      Html.script $ Html.toHtml (renderMathJaxConfig dm)
      Html.script
        Html.! HtmlA.id "MathJax-script"
        Html.! HtmlA.async ""
        Html.! HtmlA.src
                 "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"
        $      ""
      Html.link Html.! HtmlA.href (Html.toValue csspath) Html.! HtmlA.rel "stylesheet"
    Html.body d'

writeStandalone
  :: (RH.Render (b i), RH.Render i, RH.Render j) => StandaloneConfig -> Doc b j i -> Html.Html
writeStandalone sc d =
  fst $ RH.runRender (renderStandalone sc d) RH.initialRenderState
