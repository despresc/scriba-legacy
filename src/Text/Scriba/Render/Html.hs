{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Render.Html where

import           Text.Scriba.Markup

import           Control.Applicative            ( liftA2 )
import           Control.Monad.State            ( MonadState(..)
                                                , State
                                                , runState
                                                , modify
                                                , gets
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Foldable                  ( foldl' )
import qualified Data.Text                     as T
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           Text.Blaze.Html5               ( Html
                                                , (!)
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

{- TODO:

- multi-page rendering of the doc?

- standalone page rendering?

- pass along math macros?

- do I need to look at data-* or item-*?

- conditional rendering of empty things!

- Is the simple h1..h6 wrapping good enough?

-}

writeStandalone :: Doc -> Html
writeStandalone d = fst $ runRender (renderStandalone d) initialRenderState

-- Section titles run from h1 to h6, then top out there.
data RenderState = RenderState
  { rsHeaderDepth :: Int
  }

foldBy :: (Foldable t, Monoid c) => (a -> c) -> t a -> c
foldBy f = foldl' go mempty where go c a = c <> f a

renderMaybe :: Maybe Html -> (Html -> Html) -> Html
renderMaybe mh f = maybe mempty f mh

-- TODO: I _think_ the header depth should start at 1? Might need to
-- be configurable later. I think for standalone it should be 1, since
-- the document itself hopefully has a title (that we should
-- eventually gather and render).
initialRenderState :: RenderState
initialRenderState = RenderState { rsHeaderDepth = 1 }

newtype Render a = Render
  { unRender :: State RenderState a
  } deriving (Functor, Applicative, Monad, MonadState RenderState)

runRender :: Render a -> RenderState -> (a, RenderState)
runRender = runState . unRender

instance Semigroup a => Semigroup (Render a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Render a) where
  mempty = pure mempty

bumpHeaderDepth :: Render a -> Render a
bumpHeaderDepth act = do
  n <- gets rsHeaderDepth
  setDepth $ n + 1
  a <- act
  setDepth n
  pure a
  where setDepth n = modify $ \s -> s { rsHeaderDepth = n }

-- TODO: should probably remove this
-- TODO: for standalone rendering we should probably put the title of
-- the document in the header.
-- TODO: add configurability, especially re: the math.
-- TODO: have a header include option for documents. Hard-coding a
-- style path for the manual is obviously poor.
renderStandalone :: Doc -> Render Html
renderStandalone d@(Doc dm _ _ _) = do
  d' <- renderDoc d
  let tplain = docPlainTitle dm
  pure $ H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml tplain
      H.script
        ! A.id "MathJax-script"
        ! A.async ""
        ! A.src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js"
        $ ""
      H.link ! A.href "../../doc/manual.css" ! A.rel "stylesheet"
    H.body d'

-- TODO: selectively render empty sections?
-- TODO: Should the title be a Maybe?
renderDoc :: Doc -> Render Html
renderDoc (Doc t f m b) = do
  t' <- renderTitle $ docTitle t
  bumpHeaderDepth $ do
    f' <- renderSectionContent f
    m' <- renderSectionContent m
    b' <- renderSectionContent b
    pure $ H.section ! A.class_ "scribaDoc" $ do
      H.header t'
      H.section ! A.class_ "frontMatter" $ f'
      H.section ! A.class_ "mainMatter" $ m'
      H.section ! A.class_ "backMatter" $ b'

-- TODO: Distinguish the preamble from the subsections?
renderSectionContent :: SectionContent -> Render Html
renderSectionContent (SectionContent bs cs) = do
  bs' <- renderBlocks bs
  cs' <- renderSections cs
  pure $ H.div ! A.class_ "sectionContent" $ bs' <> cs'

-- TODO: add the section type as a class or data attribute

-- TODO: for untitled sections, perhaps conditionally add an anonymous
-- break? They would be necessary when we first render a sibling
-- untitled section. Some kind of state variable, I think.
renderSection :: Section -> Render Html
renderSection (Section _ _ t _ c) = do
  t' <- traverse (renderTitleWith id) t
  bumpHeaderDepth $ do
    c' <- renderSectionContent c
    pure $ H.section $ fromMaybe mempty t' <> c'

renderSections :: [Section] -> Render Html
renderSections = foldBy renderSection

renderBlock :: Block -> Render Html
renderBlock (FormalBlock fb) = renderFormalBlock fb
renderBlock (CodeBlock t) =
  pure $ H.div ! A.class_ "codeBlock" $ H.pre $ H.code $ H.toHtml t
renderBlock (ParBlock  p) = renderParagraph p
renderBlock (ListBlock b) = renderList b

-- TODO: This does conditional span/div rendering. Is that robust?
-- TODO: True for wrapping the body. Maybe better type?
renderMixedBlockBody :: Bool -> MixedBlockBody -> Render Html
renderMixedBlockBody b (BlockInlineBody ils) =
  go <$> foldBy renderParInline ils
 where
  go = case b of
    True  -> H.span ! A.class_ "body"
    False -> id
renderMixedBlockBody b (BlockBlockBody blks) = go <$> renderBlocks blks
 where
  go = case b of
    True  -> H.div ! A.class_ "body"
    False -> id

-- TODO: add the type as a data-scribaType? Though we might want that
-- to equal formalBlock here. Might want to record the number as well.
-- TODO: Should the title be "formalTitle"?
-- TODO: wrap the title separator? Also the title separator should be
-- rendered conditional on there being a title at all.
renderFormalBlock :: Formal -> Render Html
renderFormalBlock (Formal mty _ mtitle _ mtitlesep body concl) = do
  title'    <- traverse (renderInlinesWith renderTitleParts) mtitle
  titlesep' <- traverse renderInlines mtitlesep
  body'     <- renderMixedBlockBody True body
  concl'    <- traverse renderInlines concl
  let cls = "formalBlock" <> maybe "" (" " <>) mty
  pure $ H.div ! A.class_ (H.toValue cls) $ do
    renderMaybe title' $ H.span ! A.class_ "title"
    renderMaybe titlesep' $ H.span ! A.class_ "titleSep"
    body'
    renderMaybe concl' $ H.span ! A.class_ "conclusion"

renderList :: List -> Render Html
renderList b = case b of
  Ulist l -> H.ul <$> renderListItems l
  Olist l -> H.ol <$> renderListItems l
 where
  renderListItems = foldBy renderListItem
  renderListItem bs = H.li <$> renderMixedBlockBody False bs

renderParagraph :: Paragraph -> Render Html
renderParagraph (Paragraph c) = H.p <$> foldBy renderParInline c

renderParInline :: ParContent -> Render Html
renderParInline (ParInline i) = renderInline i

renderBlocks :: [Block] -> Render Html
renderBlocks = foldBy renderBlock

renderInlinesWith :: (a -> Render Html) -> [Inline a] -> Render Html
renderInlinesWith f = foldBy (renderInlineWith f)

renderInlines :: [Inline Void] -> Render Html
renderInlines = renderInlinesWith absurd

renderInlineWith :: (a -> Render Html) -> Inline a -> Render Html
renderInlineWith _ (Istr         s) = renderStr s
renderInlineWith f (Iemph        s) = renderEmph (renderInlineWith f) s
renderInlineWith f (Iquote       s) = renderQuote (renderInlineWith f) s
renderInlineWith _ (IinlineMath  s) = renderInlineMath s
renderInlineWith _ (IdisplayMath s) = renderDisplayMath s
renderInlineWith _ (Icode        s) = renderInlineCode s
renderInlineWith _ (IpageMark    s) = renderPageMark s
renderInlineWith f (Iother       s) = f s

renderInline :: Inline Void -> Render Html
renderInline = renderInlineWith absurd

renderStr :: Str -> Render Html
renderStr (Str t) = pure $ H.toHtml t

renderEmph :: (a -> Render Html) -> Emph a -> Render Html
renderEmph f (Emph i) = H.em <$> foldBy f i

renderQuote :: (a -> Render Html) -> Quote a -> Render Html
renderQuote f (Quote i) = H.q <$> foldBy f i

renderInlineMath :: InlineMath -> Render Html
renderInlineMath (InlineMath t) =
  pure $ H.span ! A.class_ "math inline" $ "\\(" <> H.toHtml t <> "\\)"

renderDisplayMath :: DisplayMath -> Render Html
renderDisplayMath d = do
  d' <- renderDisplayMathContent d
  pure $ H.span ! A.class_ "math display" $ "\\[" <> d' <> "\\]"

renderInlineCode :: InlineCode -> Render Html
renderInlineCode (InlineCode t) = pure $ H.code $ H.toHtml t

renderPageMark :: PageMark -> Render Html
renderPageMark (PageMark t) = pure $ H.span ! A.class_ "physPage" $ H.toHtml t

-- TODO: assumes mathjax or katex
renderDisplayMathContent :: DisplayMath -> Render Html
renderDisplayMathContent (Formula t) = pure $ H.toHtml t
renderDisplayMathContent (Gathered ts) =
  pure
    $  H.toHtml
    $  "\\begin{gathered}"
    <> T.intercalate "//\n" ts
    <> "\\end{gathered}"


-- | Render a heading title using the ambient header depth.

-- Add a sectionTitle class?
renderTitleWith :: (a -> TitleParts) -> Title a -> Render Html
renderTitleWith f (Title t) = do
  lvl <- gets rsHeaderDepth
  headAtLevel lvl <$> renderInlinesWith (renderTitleParts . f) t
 where
  headAtLevel n = case n of
    1 -> H.h1
    2 -> H.h2
    3 -> H.h3
    4 -> H.h4
    5 -> H.h5
    _ -> H.h6

renderTitle :: Title Void -> Render Html
renderTitle = renderTitleWith absurd

renderTitleParts :: TitleParts -> Render Html
renderTitleParts (TitlePrefix i) =
  H.span ! A.class_ "titlePrefix" <$> renderInlines i
renderTitleParts (TitleNumber i) =
  H.span ! A.class_ "number" <$> renderInlines i
renderTitleParts (TitleNote i) =
  H.span ! A.class_ "titleNote" <$> renderInlines i
renderTitleParts (TitleSep i) =
  H.span ! A.class_ "titleSep" <$> renderInlines i
renderTitleParts (TitleBody i) =
  H.span ! A.class_ "titleBody" <$> renderInlines i
