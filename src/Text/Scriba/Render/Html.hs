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
import           Data.Foldable                  ( foldl' )
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
renderStandalone :: Doc -> Render Html
renderStandalone d@(Doc dm _ _ _) = do
  d' <- renderDoc d
  let tplain = docPlainTitle dm
  pure $ H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml tplain
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

renderSection :: Section -> Render Html
renderSection (Section t c) = do
  t' <- renderTitle t
  bumpHeaderDepth $ do
    c' <- renderSectionContent c
    pure $ H.section $ t' <> c'

renderSections :: [Section] -> Render Html
renderSections = foldBy renderSection

renderBlock :: Block -> Render Html
renderBlock (FormalBlockBlock fb) = renderFormalBlock fb
renderBlock (CodeBlock        t ) = pure $ H.pre $ H.code $ H.toHtml t
renderBlock (ParBlock         p ) = renderParagraph p

renderFormalBlock :: FormalBlock -> Render Html
renderFormalBlock = undefined

renderParagraph :: Paragraph -> Render Html
renderParagraph (Paragraph c) = H.p <$> foldBy renderParInline c
  where renderParInline (ParInline i) = renderInline i

renderBlocks :: [Block] -> Render Html
renderBlocks = foldBy renderBlock

renderInlines :: [Inline] -> Render Html
renderInlines = foldBy renderInline

renderInline :: Inline -> Render Html
renderInline (Str   t) = pure $ H.toHtml t
renderInline (Emph  i) = H.em <$> renderInlines i
renderInline (Quote i) = H.q <$> renderInlines i
renderInline (Math t) =
  pure $ H.span ! A.class_ "math inline" $ "\\(" <> H.toHtml t <> "\\)"
renderInline (Code     t) = pure $ H.code $ H.toHtml t
renderInline (PageMark t) = pure $ H.span ! A.class_ "physPage" $ H.toHtml t


-- | Render a heading title using the ambient header depth.

-- Add a sectionTitle class?
renderTitle :: Title -> Render Html
renderTitle (Title t) = do
  lvl <- gets rsHeaderDepth
  headAtLevel lvl <$> renderInlines t
 where
  headAtLevel n = case n of
    1 -> H.h1
    2 -> H.h2
    3 -> H.h3
    4 -> H.h4
    5 -> H.h5
    _ -> H.h6
