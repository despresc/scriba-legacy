{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Scriba.Markup
  ( MemDoc
  , Block(..)
  , Inline(..)
  , parseMemDoc
  , prettyScribaError
  , decorateMemDoc
  , writeStandalone
  , MathJaxConfig(..)
  , StandaloneConfig(..)
  , Void1
  , absurd1
  )
where

import           Text.Scriba.Decorate
import           Text.Scriba.Element
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Aeson                     ( ToJSON(..) )
import qualified Data.Aeson                    as Aeson
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( mapMaybe )
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

-- TODO: have nil in there for note text gathering. Does that mean
-- that gathering no longer has to be type changing?
data Block b i
  = Bformal !(Formal (Block b) i)
  | Bcode !BlockCode
  | Bpar !(Paragraph i)
  | Blist !(List (Block b) i)
  | Bcontrol !(b i)
  | Bnil
  deriving (Eq, Ord, Show, Read, Generic, Functor, Numbering)

instance HasNil (Block b i) where
  embedNil = Bnil

deriving instance (FromTitleComponent i, Titling i (b i), Titling i i) => Titling i (Block b i)

instance ( Referencing i i'
         , Referencing (b i) (b' i')
         ) => Referencing (Block b i) (Block b' i') where

-- Monomorphic-ish for now
newtype BlockControl i
  = BlockNoteText (NoteText (Block BlockControl) i)
  deriving (Eq, Ord, Show, Read, Generic, Functor)
  deriving anyclass (Numbering)

instance (FromTitleComponent i, Titling i i) => Titling i (BlockControl i)

instance Gathering (NoteText (Block Void1) i') i i' => Gathering (NoteText (Block Void1) i') (Block BlockControl i) (Block Void1 i') where
  gathering (Bformal  a) = Bformal <$> gathering a
  gathering (Bcode    a) = Bcode <$> gathering a
  gathering (Bpar     a) = Bpar <$> gathering a
  gathering (Blist    a) = Blist <$> gathering a
  gathering (Bcontrol a) = gathering a
  gathering Bnil         = pure Bnil

instance Gathering (NoteText (Block Void1) i') i i' => Gathering (NoteText (Block Void1) i') (BlockControl i) (Block Void1 i') where
  gathering (BlockNoteText b) = resolveBlockNoteText b

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
  | Iref !Ref
  | ItitleComponent !(TitleComponent (Inline a))
  | Icontrol !a
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering, Titling i)

instance HasStr (Inline a) where
  embedStr = Istr

instance Gathering note a b => Gathering note (Inline a) (Inline b)

-- TODO: add a class for better instance derivation?
instance Referencing (Inline InlineControl) (Inline Void) where
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

instance Referencing InlineControl (Inline b) where
  referencing (IcRef sr) = Iref <$> resolveRef sr

newtype InlineControl
  = IcRef SourceRef
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Numbering, Titling i, Gathering note InlineControl)

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

pBlock :: Scriba Node (Inline a) -> Scriba Node (Block b (Inline a))
pBlock pInl =
  asNode
    $   Bformal
    <$> pFormal (pMixedBody (pBlock pInl) pInl) (pInlineBody pInl)
    <|> Bpar
    <$> pParagraph (pInlineBody pInl)
    <|> Bcode
    <$> pBlockCode
    <|> Blist
    <$> pList (pMixedBody (pBlock pInl) pInl)

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

pControl :: Scriba Element InlineControl
pControl = IcRef <$> pSourceRef

-- * Running parsers

-- TODO: need to have a more flexible top-level parser, recognizing
-- multiple document types. Perhaps simply by making Doc a sum
parseMemDoc
  :: Node
  -> Either ScribaError (MemDoc (Block b) (Inline a) (Inline InlineControl))
parseMemDoc = fmap snd . runScriba
  ( asNode
  $ pMemDoc pInlineCore (stripMarkup $ const []) (pBlock pInline) pInline
  )

-- * Decorating the document

getRefEnv :: GatherData note -> RefData
getRefEnv (GatherData d _) = RefData $ M.fromList $ mapMaybe go d
 where
  go (LinkNumber mi t en) = do
    i <- mi
    pure (i, (t, en))
  go LinkBare{} = Nothing

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
traverseInline f (ItitleComponent e) =
  ItitleComponent $ fmap (traverseInline f) e
traverseInline _ (Iref         e) = Iref e
traverseInline _ (Istr         s) = Istr s
traverseInline _ (IinlineMath  s) = IinlineMath s
traverseInline _ (IdisplayMath s) = IdisplayMath s
traverseInline _ (Icode        s) = Icode s
traverseInline _ (IpageMark    s) = IpageMark s

decorateMemDoc
  :: MemDoc (Block BlockControl) (Inline Void) (Inline InlineControl)
  -> Either DecorateError (MemDoc (Block Void1) (Inline Void) (Inline Void))
decorateMemDoc =
  decorating @((MemDoc (Block Void1) (Inline Void) (Inline InlineControl)))
    $ traverseInline (absurd :: Void -> Inline InlineControl)

decorating
  :: forall d' d d'' j i
   . ( HasDocAttrs j d
     , Numbering d
     , Titling i d
     , Gathering (NoteText (Block Void1) (Inline InlineControl)) d d'
     , Referencing d' d''
     )
  => (j -> i)
  -> d
  -> Either DecorateError d''
decorating f d = do
  nd <- runDocNumbering d
  td <- runDocTitling f nd
  let (d', numDat) =
        (runDocGathering :: d
            -> (d', GatherData (NoteText (Block Void1) (Inline InlineControl)))
          )
          td
  (runDocReferencing :: RefData -> d' -> Either DecorateError d'')
    (getRefEnv numDat)
    d'

-- * Rendering

-- TODO: could have a generic deriving thing here, I suppose. Would
-- want it to be output-agnostic, if possible.

instance (RH.Render (b i), RH.Render i) => RH.Render (Block b i) where
  render (Bformal  fb) = RH.render fb
  render (Bcode    t ) = RH.render t
  render (Bpar     p ) = RH.render p
  render (Blist    b ) = RH.render b
  render (Bcontrol b ) = RH.render b
  render Bnil          = mempty

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
  :: (RH.Render d, HasDocAttrs j d)
  => StandaloneConfig
  -> d
  -> RH.RenderM Html.Html
renderStandalone (StandaloneConfig csspath) d = do
  let dm = getDocAttrs d
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
  :: (RH.Render d, HasDocAttrs j d) => StandaloneConfig -> d -> Html.Html
writeStandalone sc d =
  fst $ RH.runRender (renderStandalone sc d) RH.initialRenderState
