{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , runVariedInline
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
  , TitleParts(..)
  , Varied(..)
  , VariedVar(..)
  , BlockCode(..)
  )
where

import           Text.Scriba.Counters
import           Text.Scriba.Intermediate
import           Text.Scriba.ParseIntermediate
import           Text.Scriba.Markup.DisplayMath
import           Text.Scriba.Markup.Emph
import           Text.Scriba.Markup.InlineCode
import           Text.Scriba.Markup.InlineMath
import           Text.Scriba.Markup.PageMark
import           Text.Scriba.Markup.Quote
import           Text.Scriba.Markup.Str
import           Text.Scriba.Markup.BlockCode
import           Text.Scriba.Markup.Paragraph
import           Text.Scriba.Markup.List
import           Text.Scriba.Markup.MixedBody
import           Text.Scriba.Markup.TitleParts
import           Text.Scriba.Markup.Formal

import           Control.Applicative            ( (<|>)
                                                , empty
                                                )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Char                      ( isSpace )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( mapMaybe
                                                , fromMaybe
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec                ( many )
import qualified Text.Megaparsec               as MP

{- TODO:

- Actually finish this

- Better errors everywhere (use source positions, for one thing)

- Add blocks (figure out what to do about them and paragraphs?), more
  inlines, sections.

- Clarify whitespace policy. Right now none of the parsers allow
  whitespace around elements when recognizing them. This is fine when
  the output comes from a parsed sml document, but maybe we don't want
  to rely on that. That also means that elements with unusual
  presentations might not be parsed correctly.

- Page mark (physPage) might need to be some kind of locator term, or
  at least some kind of parsed value (to support, e.g., linking to
  page images).

- for formal blocks: these have some kind of preamble thing, body
  thing, ending thing, and type. Would encompass amsthm-style
  environments, among other things. later could have templating,
  numbering, etc. Need to decide what is allowed in each position, of
  course.

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

- should some of these types become parametric? to resuse blocks
  elsewhere with different content restrictions.

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

-}

-- | A document with front matter, main matter, and end matter.
data Doc = Doc DocAttrs SectionContent SectionContent SectionContent
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: should I mapKey the docNumberStyle here?
data DocAttrs = DocAttrs
  { docTitle :: Title (Inline Void)
  , docPlainTitle :: Text
  , docTitlingConfig :: TitlingConfig
  , docElemCounterRel :: Map ContainerName CounterName
  , docCounterRel :: Map CounterName (Set CounterName)
  , docNumberStyles :: Map Text NumberStyle
  } deriving (Eq, Ord, Show, Read, Generic)

data NumberStyle = Decimal
  deriving (Eq, Ord, Show, Read, Generic)

data TitlingConfig = TitlingConfig
  { tcFormalConfig :: Map Text FormalConfig
  , tcSectionConfig :: Map Text SectionConfig
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: richer whitespace options not in the body of the template?
-- E.g. stripping all whitespace, so that the template is a little
-- more understandable.
data FormalConfig = FormalConfig
  { fconfPrefix :: Maybe [Inline Void]
  , fconfTitleTemplate :: [Varied]
  , fconfTitleSep :: Maybe [Inline Void]
  , fconfConcl :: Maybe [Inline Void]
  } deriving (Eq, Ord, Show, Read, Generic)

data SectionConfig = SectionConfig
  { sconfPrefix :: Maybe [Inline Void]
  , sconfTitleTemplate :: [Varied]
  } deriving (Eq, Ord, Show, Read, Generic)

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
data Section = Section
  { secType :: Maybe Text
  , secTitleBody :: Maybe (Title (Inline Void))
  , secTitleFull :: Maybe (Title (Inline Void))
  , secNum :: Maybe Text
  , secContent :: SectionContent
  } deriving (Eq, Ord, Show, Read, Generic)

data SectionContent = SectionContent
  { secPreamble :: [Block (Inline Void)]
  , secChildren :: [Section]
  } deriving (Eq, Ord, Show, Read, Generic)

emptySectionContent :: SectionContent
emptySectionContent = SectionContent [] []

data Block i
  = Bformal (Formal Block i)
  | Bcode BlockCode
  | Bpar (Paragraph i)
  | Blist (List Block i)
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: may want to restrict the inlines that can appear in a
-- title. May also want to have a toc title and header/running title
-- in here too. Also may want a richer title structure, say having
-- titles, separators, subtitles, that sort of thing.
newtype Title a = Title
  { titleBody :: [a]
  } deriving (Eq, Ord, Show, Read, Generic)

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
  | ItitleParts !(TitleParts (Inline a))
  | Iother !a
  deriving (Eq, Ord, Show, Read, Functor, Generic)

-- | Strip out the markup in a sequence of inlines, leaving only the
-- plain text. Very lossy, naturally. This doesn't add any textual
-- markers around code, math, or quotations, or things like that, so
-- beware.
stripMarkup :: (a -> [Text]) -> [Inline a] -> Text
stripMarkup f = T.intercalate " " . T.words . T.concat . concatMap inlineToText
 where
  inlineToText (Istr         i ) = strToText i
  inlineToText (Iemph        is) = emphToText inlineToText is
  inlineToText (Iquote       is) = quoteToText inlineToText is
  inlineToText (IinlineMath  t ) = inlineMathToText t
  inlineToText (IdisplayMath d ) = displayMathToText d
  inlineToText (Icode        t ) = inlineCodeToText t
  inlineToText (IpageMark    t ) = pageMarkToText t
  inlineToText (ItitleParts  t ) = titlePartsToText inlineToText t
  inlineToText (Iother       a ) = f a

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
  | VariedTitle
  | VariedPrefix
  | VariedNumber
  deriving (Eq, Ord, Show, Read, Generic)

pVariedSeq :: Text -> Scriba [Node] [Varied]
pVariedSeq t = firstSpace . concat <$> manyOf (pVaried t)
 where
  firstSpace (VariedSpace : xs) = firstSpace xs
  firstSpace xs                 = midSpace xs
  midSpace (VariedSpace : VariedSpace : xs) = midSpace (VariedSpace : xs)
  midSpace (VariedSpace : x           : xs) = VariedSpace : midSpace (x : xs)
  midSpace [VariedSpace                   ] = []
  midSpace (x : xs                        ) = x : midSpace xs
  midSpace []                               = []

pVaried :: Text -> Scriba Node [Varied]
pVaried t = pVariedText <|> pVariedVar t

pVariedText :: Scriba Node [Varied]
pVariedText = explodeText <$> simpleText
 where
  explodeText = map toVaried . T.split isSpace
  toVaried t | T.null t  = VariedSpace
             | otherwise = VariedStr t

-- TODO: improve error. I think I need a whileParsing here?
-- TODO: this stripping of the prefix thing is bad. Remove it, and
-- maybe add it back in if templates ever get more complex.
pVariedVar :: Text -> Scriba Node [Varied]
pVariedVar t = asNode $ do
  v <- ty $ do
    mty <- inspect
    case mty of
      Just typ -> case T.stripPrefix ("$" <> t <> ".") typ of
        Just "titlePrefix" -> pure VariedPrefix
        Just "titleNote"   -> pure VariedNote
        Just "title"       -> pure VariedTitle
        Just "n"           -> pure VariedNumber
        _                  -> throwError $ Msg "unrecognized variable"
      _ -> throwError $ Msg "unrecognized variable"
  (b, a) <- whileParsingElem (printVar v) $ meta $ attrs $ do
    bs <- mattr "before" $ allContentOf simpleText
    as <- mattr "after" $ allContentOf simpleText
    pure (T.concat bs, T.concat as)
  pure [VariedVar b v a]
 where
  printVar VariedPrefix = "titlePrefix"
  printVar VariedNote   = "titleNode"
  printVar VariedTitle  = "title"
  printVar VariedNumber = "n"

-- TODO: simply ignores undefined variables right now.
-- TODO: use of Map seems a little wasteful at the moment.
-- TODO: probably needs to wrap its components in spans!
-- TODO: do the before/after things need to be in spans too?
runVariedInline :: Map Text [Inline a] -> [Varied] -> [Inline a]
runVariedInline m = concatMap unVary
 where
  unVary VariedSpace       = [Istr $ Str " "]
  unVary (VariedStr t    ) = [Istr $ Str t]
  unVary (VariedVar b v a) = case unVaryVar v of
    Just vi -> [Istr $ Str b, vi, Istr $ Str a]
    Nothing -> []
  unVaryVar VariedPrefix =
    ItitleParts . TitlePrefix <$> M.lookup "titlePrefix" m
  unVaryVar VariedNote   = ItitleParts . TitleNote <$> M.lookup "titleNote" m
  unVaryVar VariedTitle  = ItitleParts . TitleBody <$> M.lookup "titleBody" m
  unVaryVar VariedNumber = ItitleParts . TitleNumber <$> M.lookup "n" m

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
pFormalConfig
  :: Scriba
       Element
       (Map Text (FormalConfig, Maybe ContainerRelation, Maybe NumberStyle))
pFormalConfig = whileParsingElem "formalBlocks" $ meta $ attrs $ allAttrsOf
  pFormalSpec
 where
  pFormalSpec = do
    t <-
      ty $ inspect >>= maybe (throwError $ Msg "a block type is required") pure
    whileParsingElem ("formal block " <> t <> " config") $ meta $ attrs $ do
      pref          <- attrMaybe "prefix" $ allContentOf pInline
      titleTemplate <- mattr "title" $ allContent (pVariedSeq t)
      titleSep      <- attrMaybe "titleSep" $ allContentOf pInline
      numbering     <- attrMaybe "numbering" $ pCounterDepends
      concl         <- attrMaybe "conclusion" $ allContentOf pInline
      let (counterDepends, ns) = unzips numbering
      pure
        $ (FormalConfig pref titleTemplate titleSep concl, counterDepends, ns)


-- TODO: reduce duplication with pFormalConfig
pSectionConfig
  :: Scriba
       Element
       (Map Text (SectionConfig, Maybe ContainerRelation, Maybe NumberStyle))
pSectionConfig = whileParsingElem "sections" $ meta $ attrs $ allAttrsOf
  pSectionSpec
 where
  pSectionSpec = do
    t <-
      ty
      $   inspect
      >>= maybe (throwError $ Msg "a section type is required") pure
    whileParsingElem ("section " <> t <> " config") $ meta $ attrs $ do
      pref          <- attrMaybe "prefix" $ allContentOf pInline
      titleTemplate <- mattr "title" $ allContent (pVariedSeq t)
      numbering     <- attrMaybe "numbering" $ pCounterDepends
      let (counterDepends, ns) = unzips numbering
      pure
        $ ( SectionConfig (fmap (fmap (fmap absurd)) pref) titleTemplate
          , counterDepends
          , ns
          )

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

pBlock :: Scriba Node (Block (Inline a))
pBlock =
  asNode
    $   Bformal
    <$> pFormal
    <|> Bpar
    <$> pParagraph
    <|> pCodeBlock
    <|> Blist
    <$> pList

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
pFormal :: Scriba Element (Formal Block (Inline i))
pFormal = do
  matchTy "formalBlock"
  whileParsingElem "formalBlock" $ do
    (mty, mnumber, title, note, tsep, concl) <- meta $ attrs $ do
      mty     <- attrMaybe "type" $ allContentOf simpleText
      mnumber <- attrMaybe "n" $ allContentOf simpleText
      title   <- attrMaybe "title" $ allContentOf pInline
      note    <- attrMaybe "titleNote" $ allContentOf pInline
      tsep    <- attrMaybe "titleSep" $ allContentOf pInline
      concl   <- attrMaybe "conclusion" $ allContentOf pInline
      pure (mty, mnumber, title, note, tsep, concl)
    body <- pMixedBlockBody
    pure $ Formal (T.concat <$> mty)
                  (T.concat <$> mnumber)
                  (fmap (fmap (fmap absurd)) title)
                  note
                  tsep
                  body
                  concl

-- TODO: no language attributes recognized. This is also a problem
-- with the code inline.
pCodeBlock :: Scriba Element (Block i)
pCodeBlock = do
  matchTy "codeBlock"
  t <- whileParsingElem "codeBlock" $ allContentOf simpleText
  pure $ Bcode $ BlockCode $ commonIndentStrip $ T.concat t

-- TODO: For lists, and numbered things generally, we should assume a
-- default numbering style, so that references to numbered list items
-- can still be rendered visually in HTML. Alternate styles could
-- still be set, but this would have to be done in-document, and it
-- would be the responsibility of the user (if not using whatever
-- standalone rendering we support) to ensure that the CSS matches
-- what the document assumes. We could still ship with CSS styles for
-- the numbers, and simply put different classes on the rendered
-- lists, of course.
pList :: Scriba Element (List Block (Inline a))
pList = pOlist <|> pUlist

pOlist :: Scriba Element (List Block (Inline a))
pOlist = do
  matchTy "olist"
  content $ pOnlySpace
  fmap Olist $ whileParsingElem "olist" $ allContent $ many
    (one pListItem <* pOnlySpace)

pUlist :: Scriba Element (List Block (Inline a))
pUlist = do
  matchTy "ulist"
  content $ pOnlySpace
  fmap Ulist $ whileParsingElem "ulist" $ allContent $ many
    (one pListItem <* pOnlySpace)

pListItem :: Scriba Node (MixedBody Block (Inline a))
pListItem = asNode pItem
 where
  pItem = do
    matchTy "item"
    whileParsingElem "item" $ pMixedBlockBody

pParagraph :: Scriba Element (Paragraph (Inline a))
pParagraph = do
  matchTy "p"
  c <- whileParsingElem "p" $ allContentOf pInline
  pure $ Paragraph c

pMixedBlockBody :: Scriba Element (MixedBody Block (Inline a))
pMixedBlockBody = allContent (MixedBlock <$> manyOf pBlock)
  <|> allContent (MixedInline <$> manyOf pInline)

-- ** Inline parsing

-- pInline is synonymous with pParInline
pInline :: Scriba Node (Inline a)
pInline =
  asNode
      (   pEmph
      <|> pQuote
      <|> pPageMark
      <|> pMath
      <|> pFormula
      <|> pGathered
      <|> pCode
      )
    <|> pText

pEmph :: Scriba Element (Inline a)
pEmph = do
  matchTy "emph"
  c <- whileParsingElem "emph" $ allContentOf pInline
  pure $ Iemph $ Emph c

pQuote :: Scriba Element (Inline a)
pQuote = do
  matchTy "q"
  c <- whileParsingElem "q" $ allContentOf pInline
  pure $ Iquote $ Quote c

-- TODO: well-formedness checking?
pPageMark :: Scriba Element (Inline a)
pPageMark = do
  matchTy "physPage"
  t <- whileParsingElem "physPage" $ allContentOf simpleText
  pure $ IpageMark $ PageMark $ T.concat t

pText :: Scriba Node (Inline a)
pText = Istr . Str <$> simpleText

pMath :: Scriba Element (Inline a)
pMath = do
  matchTy "math"
  ts <- whileParsingElem "math" $ allContentOf simpleText
  pure $ IinlineMath $ InlineMath $ T.concat ts

-- TODO: syntactic unification with pMath? it's probably better to
-- have a single "display" parameter control both, and have dmath be a
-- syntactic alias (in some way) for math {presentation|display}
pFormula :: Scriba Element (Inline a)
pFormula = do
  matchTy "dmath"
  c <- whileParsingElem "dmath" $ allContentOf simpleText
  pure $ IdisplayMath $ Formula $ T.concat c

-- TODO: syntact unification with formula? May want to consider design
-- here. E.g. could have a single dmath whose content is flexibly
-- parsed, have Gathered be a list of math and not Text, that sort of
-- thing.
pGathered :: Scriba Element (Inline a)
pGathered = do
  matchTy "gathered"
  c <- whileParsingElem "gathered" $ allContent $ pOnlySpace *> many
    (one pLine <* pOnlySpace)
  pure $ IdisplayMath $ Gathered c
 where
  pLine = asNode $ do
    matchTy "line"
    fmap T.concat $ whileParsingElem "line" $ allContentOf simpleText

pCode :: Scriba Element (Inline a)
pCode = do
  matchTy "code"
  t <- whileParsingElem "code" $ allContentOf simpleText
  pure $ Icode $ InlineCode $ T.concat t

-- ** Section parsing

-- For now, all things presented as sections become sections.

-- TODO: do the expectations actually work out here?
-- TODO: a top level title parser?
-- TODO: reduce duplication with pFormalConfig
pSection :: Scriba Element Section
pSection = do
  mty <- (matchTy "section" $> Just "section") <|> presentedAsSection
  whileParsingElem (fromMaybe "section of unknown type" mty) $ do
    (mtitle, mfullTitle, mnumber) <- meta $ attrs $ do
      mtitle     <- attrMaybe "title" $ allContentOf pInline
      mfullTitle <- attrMaybe "fullTitle" $ allContentOf pInline
      mnumber    <- attrMaybe "n" $ allContentOf simpleText
      pure (mtitle, mfullTitle, mnumber)
    c <- content $ pSectionContent
    pure $ Section mty
                   (Title <$> mtitle)
                   (Title . fmap (fmap absurd) <$> mfullTitle)
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
pSectionContent :: Scriba [Node] SectionContent
pSectionContent = do
  pre  <- manyOf $ pBlock
  subs <- remaining $ asNode pSection
  pure $ SectionContent pre subs

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
pDoc :: Scriba Element Doc
pDoc = do
  matchTy "scriba"
  whileParsingElem "scriba" $ do
    dm <- meta $ attrs $ do
      t       <- mattr "title" $ allContentOf pInline
      tplain <- attrMaybe "plainTitle" $ fmap T.concat $ allContentOf simpleText
      fconfig <- mattr "formalBlocks" $ pFormalConfig
      sconfig <- mattr "sections" $ pSectionConfig
      let (fconf, frelRaw, fnstyleRaw) = unzips3 fconfig
          mcrel (x, y) = (,) (ContainerName x) <$> y
          fnstyle                      = M.mapMaybe id fnstyleRaw
          (sconf, srelRaw, snstyleRaw) = unzips3 sconfig
          snstyle                      = M.mapMaybe id snstyleRaw
      (elemrel, crel) <-
        case
          compileContainerRelations
            (mapMaybe mcrel $ M.toList srelRaw <> M.toList frelRaw)
        of
          Left  e -> throwError $ Msg e
          Right a -> pure a
      pure $ DocAttrs (Title t)
                      (fromMaybe (stripMarkup absurd t) tplain)
                      (TitlingConfig fconf sconf)
                      elemrel
                      crel
                      (snstyle <> fnstyle)
    content $ pExplicitMatter dm <|> pBare dm
 where
  pMatter t = asNode $ do
    matchTy t
    whileParsingElem t $ content $ pSectionContent
  pExplicitMatter dm = do
    f <- one $ pMatter "frontMatter"
    m <- one $ pMatter "mainMatter"
    b <- one $ pMatter "endMatter"
    zero
    pure $ Doc dm f m b
  pBare dm = do
    c <- pSectionContent
    pure $ Doc dm emptySectionContent c emptySectionContent

-- * Running parsers

parseDoc :: Node -> Either ScribaError Doc
parseDoc = fmap snd . runScriba (asNode pDoc)

-- TODO: improve, especially the expectations.
-- TODO: might want to lock multiple "while parsing" lines behind a
-- --trace option in a standalone program.

-- TODO: having the source position be optional in the Expecting makes
-- the errors a little weird.
prettyScribaError :: ScribaError -> Text
prettyScribaError (WhileParsing msp t e) =
  prettyScribaError e <> "\n" <> errline
 where
  errAt =
    " at " <> maybe "<unknown position>" (T.pack . MP.sourcePosPretty) msp
  errline = "while parsing " <> t <> errAt
prettyScribaError (Expecting e mspt) = ex <> got
 where
  got = case mspt of
    Nothing      -> ""
    Just (sp, t) -> "got: " <> t <> "\nat " <> T.pack (MP.sourcePosPretty sp)
  ex = "expecting one of: " <> prettyExpectations e <> "\n"
  prettyExpectations =
    T.intercalate ", " . map fromExpectation . Set.toAscList . getExpectations
prettyScribaError (Msg t)  = "error: " <> t
prettyScribaError ErrorNil = "unknown error"
