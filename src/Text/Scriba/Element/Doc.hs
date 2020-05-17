{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Doc where

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.Section
import           Text.Scriba.Element.Str        ( HasStr(..) )
import           Text.Scriba.Element.TitleComponent
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Monad                  ( join
                                                , unless
                                                )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Char                      ( isAlpha )
import qualified Data.Map.Merge.Strict         as M
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

{- TODO:

- split this further. The titling config and numbering config should
  be bundled and put in a separate module.

-}

-- | A document with front matter, main matter, and end matter.

-- TODO: have a separate index for the attrs, so that they can have Void arguments?
data Doc b j i = Doc (DocAttrs j) (SectionContent b i) (SectionContent b i) (SectionContent b i)
  deriving (Eq, Ord, Show, Read, Generic, Functor)

-- TODO: should I mapKey the docNumberStyle here?

-- TODO: I need the numberconfig to have something like Void type, for
-- now. Otherwise I need to resolve control elements inside prefixes
-- and things
-- TODO: have languages be validated
data DocAttrs i = DocAttrs
  { docTitle :: Title i
  , docLang :: Maybe Text
  , docPlainTitle :: Text
  , docTitlingConfig :: TitlingConfig i
  , docElemCounterRel :: Map ContainerName (CounterName, NumberConfig i)
  , docCounterRel :: Map CounterName (Set CounterName)
  , docMathMacros :: Map Text (Int, Text)
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

emptySurround :: Surround a
emptySurround = Surround [] Nothing []

-- * Numbering

-- TODO: Doesn't number anything in the config. Should it?
instance (Numbering a (b i), Numbering a i) => Numbering a (Doc b j i) where
  numbering (Doc da f m b) = do
    f' <- numbering f
    m' <- numbering m
    b' <- numbering b
    pure $ Doc da f' m' b'

instance (Titling i (b i), Titling i i, FromTitleComponent i) => Titling i (Doc b j i) where
  titling (Doc da f m b) = do
    f' <- titling f
    m' <- titling m
    b' <- titling b
    pure $ Doc da f' m' b'

instance (Referencing i (f a) (g b), Referencing i a b) => Referencing i (Doc f j a) (Doc g j b) where
  referencing (Doc da f m b) = do
    f' <- referencing f
    m' <- referencing m
    b' <- referencing b
    pure $ Doc da f' m' b'

-- TODO: selectively render empty sections?
-- TODO: Should the title be a Maybe?
instance (RH.Render (b i), RH.Render i, RH.Render j) => RH.Render (Doc b j i) where
  render (Doc t f m b) = do
    t' <- RH.render $ Heading $ docTitle t
    RH.bumpHeaderDepth $ do
      f' <- RH.render f
      m' <- RH.render m
      b' <- RH.render b
      let mlang = HtmlA.lang . Html.toValue <$> docLang t
      pure $ Html.section Html.! HtmlA.class_ "scribaDoc" RH.?? mlang $ do
        Html.header t'
        Html.section Html.! HtmlA.class_ "frontMatter" $ f'
        Html.section Html.! HtmlA.class_ "mainMatter" $ m'
        Html.section Html.! HtmlA.class_ "backMatter" $ b'

-- * Parsing

-- Document attribute parsing

pFormalConfig
  :: HasStr a
  => Scriba Node a
  -> Scriba
       Element
       ( Map
           Text
           ( FormalConfig a
           , Maybe ContainerRelation
           , Maybe (NumberConfig a)
           )
       )
pFormalConfig pInl = meta $ attrs $ allAttrsOf pFormalSpec
 where
  pFormalSpec = do
    t <-
      ty $ inspect >>= maybe (throwError $ Msg "a block type is required") pure
    whileParsingElem ("formal block " <> t <> " config") $ meta $ attrs $ do
      titleTemplate <- attrMaybe "title" $ meta $ attrs
        (pTitleTemplate pInl FormalTemplate)
      titleSep <- attrMaybe "titleSep" $ allContentOf pInl
      (cr, nc) <- pNumberRef pInl
      concl    <- attrMaybe "conclusion" $ allContentOf pInl
      pure
        ( FormalConfig titleTemplate titleSep concl
        , cr
        , nc <*> pure FilterByCounterDep
        )

defaultListConfig :: HasStr a => NumberConfig a
defaultListConfig = NumberConfig
  ( NumberStyle (FilterByContainer "item:olist") Nothing
  $ DepthStyle [Decimal, LowerAlpha, LowerRoman, Decimal]
  )
  (Just [embedStr "item"])
  (Just [embedStr " "])

pListConfig :: HasStr a => Scriba Node a -> Scriba Element (NumberConfig a)
pListConfig _ =
  meta $ attrs $ attrDef "olist" defaultListConfig $ pure defaultListConfig

pTitleTemplate
  :: HasStr a
  => Scriba Node a
  -> TitleTemplateStyle
  -> Scriba Attrs (TitleTemplate a)
pTitleTemplate pInl t = do
  pref        <- attrDef "prefix" emptySurround $ pSurround pInl
  tnote       <- attrDef tname emptySurround $ pSurround pInl
  tnum        <- attrDef "n" emptySurround $ pSurround pInl
  prefixFirst <- attrMaybe "prefixFirst" $ pure True
  numberFirst <- attrMaybe "numberFirst" $ pure False
  pure $ TitleTemplate pref
                       tnum
                       tnote
                       [embedStr " "]
                       (fromMaybe True $ prefixFirst <|> numberFirst)
 where
  tname = case t of
    FormalTemplate  -> "note"
    SectionTemplate -> "titleBody"

pSurround :: Scriba Node a -> Scriba Element (Surround a)
pSurround pInl = do
  (b, a) <- meta $ attrs $ do
    b <- mattr "before" $ allContentOf pInl
    a <- mattr "after" $ allContentOf pInl
    pure (b, a)
  c <- allContentOf pInl
  let c' = case c of
        [] -> Nothing
        x  -> Just x
  pure $ Surround b c' a

-- TODO: I need to recreate this for list numbering parsing anyway, so
-- we probably don't need to return a Maybe function here. Just have
-- the filter be by relatedness.
pNumberRef
  :: Scriba Node a
  -> Scriba
       Attrs
       ( Maybe ContainerRelation
       , Maybe (ContainerPathFilter -> NumberConfig a)
       )
pNumberRef pInl = do
  numberingConf     <- attrMaybe "numbering" pCounterDepends
  (refSep, refPref) <- fmap unzips $ attrMaybe "ref" $ meta $ attrs $ do
    s <- attrMaybe "sep" $ allContentOf pInl
    p <- attrMaybe "prefix" $ allContentOf pInl
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
  :: HasStr a
  => Scriba Node a
  -> Scriba
       Element
       ( Map
           Text
           ( SectionConfig a
           , Maybe ContainerRelation
           , Maybe (NumberConfig a)
           )
       )
pSectionConfig pInl = meta $ attrs $ allAttrsOf pSectionSpec
 where
  pSectionSpec = do
    t <-
      ty
      $   inspect
      >>= maybe (throwError $ Msg "a section type is required") pure
    whileParsingElem ("section " <> t <> " config") $ meta $ attrs $ do
      titleTemplate <- attrMaybe "title" $ meta $ attrs
        (pTitleTemplate pInl SectionTemplate)
      (cr, nc) <- pNumberRef pInl
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
pDoc
  :: HasStr j
  => Scriba Node j
  -> ([j] -> Text)
  -> Scriba Node (b i)
  -> Scriba Node i
  -> Scriba Element (Doc b j i)
pDoc pMetInl stripMarkup pBlk pInl = do
  matchTy "scriba"
  whileParsingElem "scriba" $ do
    dm <- meta $ attrs $ do
      t       <- mattr "title" $ allContentOf pMetInl
      mlang <- attrMaybe "lang" $ T.strip . T.concat <$> allContentOf simpleText
      mmacros <- mattr "mathMacros" $ meta $ attrs pMathMacros
      tplain  <- attrMaybe "plainTitle" $ T.concat <$> allContentOf simpleText
      fconfig <- mattr "formalBlocks" (pFormalConfig pMetInl)
      sconfig <- mattr "sections" (pSectionConfig pMetInl)
      lconfig <- attrDef "lists" defaultListConfig (pListConfig pMetInl)
      let lCrel = ("item:olist", Just $ Relative [])
      (dmathCrel, dmathNumConf) <-
        fmap unzips $ attrMaybe "formula" $ meta $ attrs (pNumberRef pMetInl)
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
                      (fromMaybe (stripMarkup t) tplain)
                      (TitlingConfig fconf sconf)
                      elemrel'
                      crel
                      mmacros
    content $ pExplicitMatter dm <|> pBare dm
 where
  pMatter t = asNode $ do
    matchTy t
    whileParsingElem t $ content $ pSectionContent pBlk pInl
  pExplicitMatter dm = do
    f <- one $ pMatter "frontMatter"
    m <- one $ pMatter "mainMatter"
    b <- one $ pMatter "backMatter"
    zero
    pure $ Doc dm f m b
  pBare dm = do
    c <- pSectionContent pBlk pInl
    pure $ Doc dm emptySectionContent c emptySectionContent
