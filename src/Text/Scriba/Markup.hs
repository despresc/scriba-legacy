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
  )
where

import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Linking
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Aeson                     ( ToJSON(..) )
import qualified Data.Aeson                    as Aeson
import           Data.Map.Strict                ( Map )
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
  deriving (Eq, Ord, Show, Read, Generic, Functor, Numbering, Linking)

deriving instance (FromTitleComponent i, Titling i i) => Titling i (Block i)
instance Referencing (Inline a) (Inline b) => Referencing (Block (Inline a)) (Block (Inline b))

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
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering, Titling i, Linking)

instance HasStr (Inline a) where
  embedStr = Istr

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
  deriving anyclass (Numbering, Titling i, Linking)

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

pBlock :: Scriba Node (Inline a) -> Scriba Node (Block (Inline a))
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
  :: Node -> Either ScribaError (MemDoc Block (Inline a) (Inline InlineControl))
parseMemDoc = fmap snd . runScriba
  ( asNode
  $ pMemDoc pInlineCore (stripMarkup $ const []) (pBlock pInline) pInline
  )

-- * Decorating the document

getRefEnv :: NumberData -> RefData
getRefEnv (NumberData d) = RefData $ M.fromList $ go <$> d
  where go (NumberDatum i cn nc num) = (i, (cn, nc, num))

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
  :: MemDoc Block (Inline Void) (Inline InlineControl)
  -> Either DecorateError (MemDoc Block (Inline Void) (Inline Void))
decorateMemDoc =
  decorating $ traverseInline (absurd :: Void -> Inline InlineControl)

decorating
  :: forall d d' j i
   . (HasDocAttrs j d, Numbering d, Titling i d, Referencing d d', Linking d)
  => (j -> i)
  -> d
  -> Either DecorateError d'
decorating f d = do
  (numDat, nd) <- runDocNumbering d
  td           <- runDocTitling f nd
  let _ = runDocLinking td
  runDocReferencing (getRefEnv numDat) td

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
