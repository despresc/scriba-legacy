{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Scriba.Markup.Parse where

import           Text.Scriba.Markup.Surface
import qualified Text.Scriba.Intermediate      as I
import           Text.Scriba.Intermediate
                                         hiding ( Meta
                                                , content
                                                , label
                                                )


import           Control.Applicative            ( optional )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Bifunctor                 ( second )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void                      ( Void )

{- TODO:

- allow custom block and inline elements. Indeed, the parsers should
  probably be, e.g. `pParagraph :: FromElement i => Scriba Element
  (Paragraph i)` and so on, with
  FromElement { pElement :: Scriba Element a) }

- "type inference", at least in the simple case of omitting the type
  of elements whose parents have uniform content (the `item` in an
  olist or ulist, for instance)

- tight list (only inline content in the items) and loose list (block
  content in the items) distinction?

- need to decide on consistent presentation of symbolic
  things. E.g. pCitation looks in the body of the `target` attribute
  for the target, not the first argumentq

- better pFromTyped error (e.g. might want to throw expectsGotAt [list
  of keys] in case of failure)

- pNamePart might want a generic "{namePart {type|thing}}" form,
  instead of the current {family|...}, {given|...}.

- better pSourceRef validation

- still need to improve the pArticle errors (coming from pMatter), and
  the pSection errors.

-}

-- | Types that can be decoded from scriba elements, and whose
-- representations can be recognized by the element type alone.
class FromTyped a where
  fromTyped :: [(Text, Scriba Element a)]

pFromTyped :: FromTyped a => Scriba Element a
pFromTyped = do
  mty <- I.ty inspect
  case mty of
    Nothing -> empty
    Just t  -> case HashMap.lookup t pmap of
      Nothing -> empty
      Just p  -> whileParsingElem t p
  where pmap = HashMap.fromList fromTyped

instance FromTyped Void where
  fromTyped = []

instance FromTyped (Void1 a) where
  fromTyped = []

instance FromTyped i => FromTyped (InlineControl i) where
  fromTyped = controlInlineParsers <> (second (fmap IcCustom) <$> fromTyped)

instance FromTyped i => FromTyped (Inline i) where
  fromTyped = coreInlineParsers <> (second (fmap Icustom) <$> fromTyped)

instance (FromTyped (b i), FromMixed i) => FromTyped (BlockControl b i) where
  fromTyped = controlBlockParsers <> (second (fmap BcCustom) <$> fromTyped)

instance (FromTyped (b i), FromMixed i) => FromTyped (Block b i) where
  fromTyped = coreBlockParsers <> (second (fmap Bcustom) <$> fromTyped)

-- | Types that can be decoded from a stream of scriba nodes.
class FromMixed a where
  fromMixed :: Scriba [Node] a

instance FromMixed Str where
  fromMixed = pStr

instance FromTyped i => FromMixed (Inline i) where
  fromMixed = pInline

instance (FromMixed i, FromTyped (b i)) => FromMixed (Block b i) where
  fromMixed = pBlock

manyMixed :: FromMixed a => Scriba [Node] [a]
manyMixed = many fromMixed

allMixed :: FromMixed a => Scriba [Node] [a]
allMixed = manyMixed <* zero

allMixedContent :: FromMixed a => Scriba Element [a]
allMixedContent = I.content allMixed

type SurfB f b i = f (Block (BlockControl b)) (Inline i)

type SurfI f i = f (InlineControl i)

-- * Utility parsers

oneMixedSymbol :: Scriba [Node] Text
oneMixedSymbol = do
  consumeWhiteSpace
  t <- one simpleText
  consumeWhiteSpace
  zero
  pure t

-- * Document parsers

pDoc :: (FromMixed i, FromMixed (b i)) => Scriba Element (Doc b i)
pDoc = whileMatchTy "scriba" $ do
  t              <- meta $ I.attrs $ attr "type" $ I.content oneMixedSymbol
  univAttrs      <- meta $ I.attrs pUnivAttrs
  title          <- meta $ I.attrs $ mattr "title" allMixedContent
  biblioAttrs    <- meta $ I.attrs pBiblioAttrs
  numberingAttrs <- meta $ I.attrs pNumberingAttrs
  titlingAttrs   <- meta $ I.attrs pTitlingAttrs
  case t of
    "article" -> do
      content <- DocArticle <$> pArticle
      pure Doc { .. }
    _ -> throwError $ Msg $ "unknown document type: " <> t

pUnivAttrs :: Scriba Attrs UnivAttrs
pUnivAttrs = do
  label <- attrMaybe "id" $ I.content pLabel
  lang  <- attrMaybe "lang" $ I.content pLang
  pure UnivAttrs { .. }

pLabel :: Scriba [Node] Identifier
pLabel = Identifier <$> oneMixedSymbol

pLang :: Scriba [Node] Lang
pLang = Lang <$> oneMixedSymbol

pBiblioAttrs :: Scriba Attrs BiblioAttrs
pBiblioAttrs = do
  titleInfo  <- attr "titleInfo" pBiblioTitle
  nameInfo   <- mattr "nameInfo" pBiblioNames
  originInfo <- attrDef "originInfo" nullBiblioOrigin pBiblioOrigin
  let plainTitle = ""
  pure BiblioAttrs { .. }

pBiblioTitle :: Scriba Element BiblioTitle
pBiblioTitle = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- allContentOf $ asNode pBiblioTitlePart
  pure BiblioTitle { .. }
 where
  pBiblioTitlePart = do
    partType <- ty $ do
      t <- inspect
      case t of
        Just "mainTitle" -> pure MainTitle
        Just "subTitle"  -> pure SubTitle
        Just "nonSort"   -> pure TitleNonSort
        Just t'          -> throwError $ Msg $ "unrecognized title part " <> t'
        _                -> throwError $ Msg "untyped title part"
    univAttrs <- meta $ I.attrs pUnivAttrs
    content   <- Text.concat <$> allContentOf simpleText
    pure BiblioTitlePart { .. }


pBiblioNames :: Scriba Element [BiblioName]
pBiblioNames = allContentOf $ asNode pBiblioName
 where
  pNameType = attrDef "type" PersonalName $ do
    t <- I.content oneMixedSymbol
    case t of
      "personal"   -> pure PersonalName
      "corporate"  -> pure CorporateName
      "conference" -> pure ConferenceName
      "family"     -> pure FamilyName
      _            -> throwError $ Msg $ "unrecognized name type " <> t
  pNamePart = do
    univAttrs <- meta $ I.attrs pUnivAttrs
    partType  <- ty $ do
      t <- inspect
      case t of
        Just "family"        -> pure FamilyNamePart
        Just "given"         -> pure GivenName
        Just "date"          -> pure DateName
        Just "termOfAddress" -> pure TermsOfAddress
        Just t' -> throwError $ Msg $ "unrecognized name part " <> t'
        Nothing              -> throwError $ Msg "untyped name part"
    content <- Text.concat <$> allContentOf simpleText
    pure NamePart { .. }
  pBiblioName = whileMatchTy "name" $ do
    univAttrs <- meta $ I.attrs pUnivAttrs
    nameType  <- meta $ I.attrs pNameType
    let authority = Nothing
    content <- allContentOf $ asNode pNamePart
    pure BiblioName { .. }

pBiblioOrigin :: Scriba Element BiblioOrigin
pBiblioOrigin = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  place <- meta $ I.attrs $ attrMaybe "place" $ pBiblioOriginContent OriginPlace
  publisher <- meta $ I.attrs $ attrMaybe "publisher" $ pBiblioOriginContent
    OriginPublisher
  dateIssued <- meta $ I.attrs $ attrMaybe "dateIssued" $ pBiblioOriginContent
    OriginDateIssued
  edition <- meta $ I.attrs $ attrMaybe "edition" $ pBiblioOriginContent
    OriginEdition
  pure BiblioOrigin { .. }
 where
  pBiblioOriginContent f = do
    univAttrs <- meta $ I.attrs pUnivAttrs
    content   <- Text.concat <$> allContentOf simpleText
    pure $ f univAttrs content

pNumberingAttrs :: Scriba Attrs NumberingAttrs
pNumberingAttrs = undefined

pTitlingAttrs :: Scriba Attrs TitlingAttrs
pTitlingAttrs = pure TitlingAttrs

pArticle :: (FromMixed i, FromMixed (b i)) => Scriba Element (Article b i)
pArticle = I.content $ pExplicitMatter <|> pBare
 where
  pExplicitMatter = do
    frontMatter <- one $ asNode pFrontMatter
    mainMatter  <- one $ asNode pMainMatter
    pure Article { .. }
  pBare = do
    b <- manyMixed
    let frontMatter = [Introduction nullUnivAttrs nullSecAttrs b]
    mainMatter <- remaining $ asNode pSection
    pure Article { .. }

-- Does not match the type at all, so can be used elsewhere
pSection :: (FromMixed (b i), FromMixed i) => Scriba Element (Section b i)
pSection = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  secAttrs  <- meta $ I.attrs pSecAttrs
  preamble  <- I.content manyMixed
  children  <- I.content $ remaining $ asNode pSubsection
  pure Section { .. }

pSubsection :: (FromMixed (b i), FromMixed i) => Scriba Element (Subsection b i)
pSubsection = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  secAttrs  <- meta $ I.attrs pSecAttrs
  content   <- I.content manyMixed
  pure Subsection { .. }

pFrontMatter
  :: (FromMixed (b i), FromMixed i) => Scriba Element [FrontMatter b i]
pFrontMatter = whileMatchTy "frontMatter" $ do
  undefined

pMainMatter :: (FromMixed (b i), FromMixed i) => Scriba Element [Section b i]
pMainMatter = whileMatchTy "mainMatter" $ do
  undefined

pRawNum :: Scriba Element RawNum
pRawNum = RawNum . Text.concat <$> allContentOf simpleText

pPageTitle :: Scriba Element PageTitle
pPageTitle = PageTitle <$> I.content oneMixedSymbol

pSecAttrs :: FromMixed i => Scriba Attrs (SecAttrs i)
pSecAttrs = do
  titleBody <- attrMaybe "title" allMixedContent
  titleFull <- attrMaybe "fullTitle" allMixedContent
  pageTitle <- attrMaybe "pageTitle" pPageTitle
  num       <- attrMaybe "n" pRawNum
  pure SecAttrs { .. }

-- * Block Parsers

manyBlocks :: (FromMixed i, FromTyped (b i)) => Scriba [Node] [Block b i]
manyBlocks = many pBlock

remainingBlocks :: (FromMixed i, FromTyped (b i)) => Scriba [Node] [Block b i]
remainingBlocks = manyBlocks <* zero

pBlockElement :: (FromMixed i, FromTyped (b i)) => Scriba Element (Block b i)
pBlockElement = pFromTyped

pBlock :: (FromMixed i, FromTyped (b i)) => Scriba [Node] (Block b i)
pBlock = one (asNode pBlockElement) <* consumeWhiteSpace

coreBlockParsers
  :: (FromMixed i, FromTyped (b i)) => [(Text, Scriba Element (Block b i))]
coreBlockParsers =
  [ ("formal"     , Bformal <$> pFormal)
  , ("p"          , Bpar <$> pParagraph)
  , ("blockCode"  , Bcode <$> pBlockCode)
  , ("olist"      , Bolist <$> pOlist)
  , ("ulist"      , Bulist <$> pUlist)
  , ("simpleTable", BsimpleTable <$> pSimpleTable)
  ]

controlBlockParsers
  :: (FromMixed i, FromTyped (b i))
  => [(Text, Scriba Element (BlockControl b i))]
controlBlockParsers = [("noteText", BcNoteText <$> pNoteText)]

pFormal :: (FromMixed i, FromMixed (b i)) => Scriba Element (Formal b i)
pFormal = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  typ       <-
    meta $ I.attrs $ attrMaybe "type" $ Text.concat <$> allContentOf simpleText
  num       <- meta $ I.attrs $ attrMaybe "n" pRawNum
  titleFull <- meta $ I.attrs $ attrMaybe "title" allMixedContent
  titleNote <- meta $ I.attrs $ attrMaybe "titleNote" allMixedContent
  titleSep  <- meta $ I.attrs $ attrMaybe "titleSep" allMixedContent
  content   <- I.content pMixedBody
  pure Formal { .. }

pParagraph :: FromMixed i => Scriba Element (Paragraph i)
pParagraph = do
  etp <- eitherP (matchTy "p") presentedAsParagraph
  let e = case etp of
        Left  _ -> "p"
        Right _ -> "paragraph block"
  whileParsingElem e $ do
    univAttrs <- meta $ I.attrs pUnivAttrs
    content   <- allMixedContent
    pure Paragraph { .. }
 where
  presentedAsParagraph = meta $ do
    I.Meta _ pres _ _ <- inspect
    case pres of
      AsPara -> pure ()
      _      -> empty

pBlockCode :: Scriba Element BlockCode
pBlockCode = whileMatchTy "codeBlock" $ do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- commonIndentStrip . Text.concat <$> allContentOf simpleText
  pure BlockCode { .. }

pOlist :: (FromMixed (b i), FromMixed i) => Scriba Element (Olist b i)
pOlist = whileMatchTy "olist" $ do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- allContent $ consumeWhiteSpace *> many
    (one pOlistItem <* consumeWhiteSpace)
  pure Olist { .. }
 where
  pOlistItem = asNode $ whileMatchTy "item" $ do
    univAttrs <- meta $ I.attrs pUnivAttrs
    num       <- meta $ I.attrs $ attrMaybe "n" pRawNum
    content   <- I.content pMixedBody
    pure OlistItem { .. }

pUlist :: (FromMixed (b i), FromMixed i) => Scriba Element (Ulist b i)
pUlist = whileMatchTy "ulist" $ do
  univAttrs <- meta $ I.attrs pUnivAttrs
  let marker = ()
  content <- I.content $ consumeWhiteSpace *> many
    (one pUlistItem <* consumeWhiteSpace)
  pure Ulist { .. }
 where
  pUlistItem = asNode $ whileMatchTy "item" $ do
    univAttrs <- meta $ I.attrs pUnivAttrs
    content   <- I.content pMixedBody
    pure UlistItem { .. }

pSimpleTable :: FromMixed i => Scriba Element (SimpleTable b i)
pSimpleTable = do
  colSpec   <- meta $ attrs $ mattr "columns" pColSpec
  tableHead <- fromMaybe nullSimpleTableHead <$> optional pSimpleTableHead
  tableBody <- fromMaybe nullSimpleTableBody <$> optional pSimpleTableBody
  let tbl = SimpleTable { .. }
  guardValidTable tbl
  pure tbl

pColSpec :: Scriba Element ColSpec
pColSpec = meta $ args $ ColSpec <$> remaining pColData
 where
  pAlign = do
    t <- ty inspect
    case t of
      Just "left"    -> pure AlignLeft
      Just "right"   -> pure AlignRight
      Just "center"  -> pure AlignCenter
      Just "default" -> pure AlignDefault
      Just x ->
        throwError $ Msg $ "column alignment: " <> x <> " not recognized"
      Nothing -> throwError $ Msg "column alignment must be present"
  pColData = ColData <$> asNode pAlign

pSimpleTableHead :: FromMixed i => Scriba Element (SimpleTableHead i)
pSimpleTableHead = whileMatchTy "head" $ do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- allContentOf $ asNode pHeadRow
  pure SimpleTableHead { .. }
 where
  pHeadRow = whileMatchTy "row" $ do
    univAttrs <- meta $ I.attrs pUnivAttrs
    content   <- allContentOf $ asNode pHeadCell
    pure HeadRow { .. }
  pHeadCell = whileMatchTy "cell" $ do
    univAttrs <- meta $ I.attrs pUnivAttrs
    content   <- allMixedContent
    pure HeadCell { .. }

pSimpleTableBody :: FromMixed i => Scriba Element (SimpleTableBody i)
pSimpleTableBody = whileMatchTy "body" $ do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- allContentOf $ asNode pBodyRow
  pure SimpleTableBody { .. }
 where
  pBodyRow = whileMatchTy "row" $ do
    univAttrs <- meta $ I.attrs pUnivAttrs
    content   <- allContentOf $ asNode pBodyCell
    pure BodyRow { .. }
  pBodyCell = whileMatchTy "cell" $ do
    univAttrs <- meta $ I.attrs pUnivAttrs
    content   <- allMixedContent
    pure BodyCell { .. }

guardValidTable :: SimpleTable b i -> Scriba Element ()
guardValidTable = const $ pure ()

pMixedBody :: (FromMixed i, FromMixed (b i)) => Scriba [Node] (MixedBody b i)
pMixedBody = BlockBody <$> allMixed <|> InlineBody <$> allMixed

pNoteText :: (FromMixed (b i), FromMixed i) => Scriba Element (NoteText b i)
pNoteText = whileMatchTy "noteText" $ do
  univAttrs <- meta $ I.attrs pUnivAttrs
  let num = Nothing
  content <- I.content pMixedBody
  pure NoteText { .. }

-- * Inline Parsers

pStr :: Scriba [Node] Str
pStr = Str . Text.concat <$> manyOf simpleText

pStr' :: Scriba [Node] (Inline i)
pStr' = Istr <$> pStr

pEmph :: FromMixed i => Scriba Element (Emph i)
pEmph = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- allMixedContent
  pure Emph { .. }

pQuote :: FromMixed i => Scriba Element (Quote i)
pQuote = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- allMixedContent
  pure Quote { .. }

pWorkTitle :: FromMixed i => Scriba Element (WorkTitle i)
pWorkTitle = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- allMixedContent
  pure WorkTitle { .. }

pRegularize :: FromMixed i => Scriba Element (Regularize i)
pRegularize = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  old       <- meta $ I.attrs $ mattr "old" allMixedContent
  new       <- meta $ I.attrs $ mattr "new" allMixedContent
  pure Regularize { .. }

pTarget :: Scriba Element Identifier
pTarget = I.content pLabel

pCitation :: FromMixed i => Scriba Element (Citation i)
pCitation = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  target    <- meta $ I.attrs $ attrMaybe "target" pTarget
  content   <- allMixedContent
  pure Citation { .. }

pForeign :: FromMixed i => Scriba Element (Foreign i)
pForeign = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- allMixedContent
  pure Foreign { .. }

pInlineMath :: Scriba Element InlineMath
pInlineMath = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- Text.concat <$> allContentOf simpleText
  pure InlineMath { .. }

pFormula :: Scriba Element DisplayMath
pFormula = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  Formula univAttrs <$> pMathItem

pIsNumbered :: Scriba Attrs Bool
pIsNumbered = fmap isNothing $ attrMaybe "noNum" $ pure ()

pGathered :: Scriba Element DisplayMath
pGathered = do
  univAttrs  <- meta $ I.attrs pUnivAttrs
  isNumbered <- meta $ I.attrs pIsNumbered
  content    <-
    allContent $ consumeWhiteSpace *> many pLines <* consumeWhiteSpace <* zero
  pure $ Gather univAttrs isNumbered content
 where
  pLines = one (asNode pLine) <* consumeWhiteSpace
  pLine  = whileMatchTy "line" pMathItem

pMathItem :: Scriba Element MathItem
pMathItem = do
  univAttrs  <- meta $ I.attrs pUnivAttrs
  num        <- meta $ I.attrs $ attrMaybe "n" pRawNum
  isNumbered <- meta $ I.attrs pIsNumbered
  content    <- Text.concat <$> allContentOf simpleText
  pure MathItem { .. }

pInlineCode :: Scriba Element InlineCode
pInlineCode = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- Text.concat <$> allContentOf simpleText
  pure InlineCode { .. }

pPageMark :: Scriba Element PageMark
pPageMark = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  content   <- Text.concat <$> allContentOf simpleText
  pure PageMark { .. }

pSourceNoteMark :: Scriba Element SourceNoteMark
pSourceNoteMark = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  target    <- meta $ I.attrs $ attr "target" pTarget
  pure SourceNoteMark { .. }

pSourceRef :: Scriba Element SourceRef
pSourceRef = do
  univAttrs <- meta $ I.attrs pUnivAttrs
  t         <- I.content oneMixedSymbol
  let target = LibUrl { domain = Nothing, path = mkPath $ Text.splitOn "." t }
  pure SourceRef { .. }
  where mkPath ps = ["item"] <> fmap LibUrlPart ps

coreInlineParsers :: FromTyped i => [(Text, Scriba Element (Inline i))]
coreInlineParsers =
  [ ("emph"    , Iemph <$> pEmph)
  , ("q"       , Iquote <$> pQuote)
  , ("title"   , IworkTitle <$> pWorkTitle)
  , ("reg"     , Iregularize <$> pRegularize)
  , ("citation", Icitation <$> pCitation)
  , ("foreign" , Iforeign <$> pForeign)
  , ("math"    , IinlineMath <$> pInlineMath)
  , ("dmath"   , IdisplayMath <$> pFormula)
  , ("gathered", IdisplayMath <$> pGathered)
  , ("code"    , Icode <$> pInlineCode)
  , ("physPage", IpageMark <$> pPageMark)
  ]

controlInlineParsers
  :: FromTyped i => [(Text, Scriba Element (InlineControl i))]
controlInlineParsers = [("noteMark", IcNoteMark <$> pSourceNoteMark)]

pInlineElement :: FromTyped i => Scriba Element (Inline i)
pInlineElement = pFromTyped

pInline :: FromTyped i => Scriba [Node] (Inline i)
pInline = one (asNode pInlineElement) <|> pStr'

manyInlines :: FromTyped i => Scriba [Node] [Inline i]
manyInlines = many pInline

remainingInlines :: FromTyped i => Scriba [Node] [Inline i]
remainingInlines = manyInlines <* zero

{- TODO: 

Something like this for the parsers. Possibly more than one
class. Should have zeroOne, zeroMany, oneMany parsers for lists of
things, which can accumulate their expectations.

type Ety = Text -- Element type
type Pres = Text -- Element presentation
type Meta = Text -- Element metadata, not including type and presentation
type Err = Text -- Allowed errors
type Expect = Text -- The expected thing
type AttrKey = Text -- The key of an attribute

class (forall a. Applicative (f a)) => Parser f where
--  eval :: f s a -> s -> Either Err a
  onElem :: Expect
         -> ((Ety, Pres) -> Maybe a)
         -> (a -> Meta -> [Node] -> Either Err b)
         -> f Element b
  oneElementOf :: Foldable t => t (Ety, Meta -> [Node] -> Either Err a) -> f Element a
  exclusive :: Traversable t => t (f a b) -> f a b
  mixedContent :: HasStr a => f Element a -> f [Node] a
  exactAttrs :: f Attrs a -> f Element a
  laxAttrs :: f Attrs a -> f Element a
  onAttr :: AttrKey -> (Meta -> [Node] -> Either Err a) -> f Attr a

class HasStr a where
  embedStr :: Text -> a

class FromMixedContent a where
  pMixedContent :: Parser f => f [Node] [a]

-}
