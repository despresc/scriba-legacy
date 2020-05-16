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

data Block a
  = Bformal !(Formal Block a)
  | Bcode !BlockCode
  | Bpar !(Paragraph a)
  | Blist !(List Block a)
  deriving (Eq, Ord, Show, Read, Generic, Functor, Numbering i)

deriving instance (FromTitleComponent i, Titling i i) => Titling i (Block i)
instance Referencing i (Inline a) (Inline b) => Referencing i (Block (Inline a)) (Block (Inline b))

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

-- TODO: add a class for better instance derivation?
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

pListConfig :: Scriba Element (NumberConfig (Inline a))
pListConfig =
  meta $ attrs $ attrDef "olist" defaultListConfig $ pure defaultListConfig

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

-- TODO: I need to recreate this for list numbering parsing anyway, so
-- we probably don't need to return a Maybe function here. Just have
-- the filter be by relatedness.
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
-- TODO: the pBare dm and pExplicitMatter dm thing is a bit bad.
-- TODO: For error purposes it might be better if the meta is in a
-- whileParsing "document meta", and the body is in a whileParsing
-- "document body", but this is somewhat stylistic.
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
      mlang <- attrMaybe "lang" $ T.strip . T.concat <$> allContentOf simpleText
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
                      mlang
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

runNumDoc
  :: Numbering (Inline j) a
  => Doc Block (Inline j) (Inline a)
  -> Either
       DecorateError
       (NumberData (Inline j), Doc Block (Inline j) (Inline a))
runNumDoc d@(Doc da _ _ _) =
  flip runNumberM (defaultNumberState da) $ numbering d

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

-- TODO: Have StandaloneConfig instead be (StandaloneConfig a) and
-- make it an HTML instance (with a content :: a and the rest of the
-- config as other fields)? If we are keeping it, of course.
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
      Html.link Html.! HtmlA.href (Html.toValue csspath) Html.! HtmlA.rel
        "stylesheet"
    Html.body d'

writeStandalone
  :: (RH.Render (b i), RH.Render i, RH.Render j)
  => StandaloneConfig
  -> Doc b j i
  -> Html.Html
writeStandalone sc d =
  fst $ RH.runRender (renderStandalone sc d) RH.initialRenderState
