{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Render.Html where

import           Text.Scriba.Markup

import           Data.Foldable                  ( traverse_ )
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

- for sections, may want to wrap its body in a div, once there are
  titles and things.
-}

-- TODO: should probably remove this
renderStandalone :: Doc -> Html
renderStandalone d = H.docTypeHtml $ do
  H.head $ do
    H.title "standalone"
  H.body $ do
    renderDoc d

-- TODO: selectively render empty sections?
renderDoc :: Doc -> Html
renderDoc (Doc f m b) = H.section ! A.class_ "scribaDoc" $ do
  H.section ! A.class_ "frontMatter" $ renderSection f
  H.section ! A.class_ "mainMatter" $ renderSection m
  H.section ! A.class_ "backMatter" $ renderSection b

-- TODO: Distinguish the preamble from the subsections?
renderSection :: Section -> Html
renderSection (Section bs cs) = H.section $ do
  renderBlocks bs
  renderSections cs

renderSections :: [Section] -> Html
renderSections = traverse_ renderSection

renderBlock :: Block -> Html
renderBlock (FormalBlockBlock fb) = renderFormalBlock fb
renderBlock (CodeBlock        t ) = H.pre $ H.code $ H.toHtml t
renderBlock (ParBlock         p ) = renderParagraph p

renderFormalBlock :: FormalBlock -> Html
renderFormalBlock = undefined

renderParagraph :: Paragraph -> Html
renderParagraph (Paragraph c) = H.p $ traverse_ renderParInline c
  where renderParInline (ParInline i) = renderInline i

renderBlocks :: [Block] -> Html
renderBlocks = traverse_ renderBlock

renderInlines :: [Inline] -> Html
renderInlines = traverse_ renderInline

renderInline :: Inline -> Html
renderInline (Str  t) = H.toHtml t
renderInline (Emph i) = H.em $ renderInlines i
renderInline (Math t) =
  H.span ! A.class_ "math inline" $ "\\(" <> H.toHtml t <> "\\)"
renderInline (Code     t) = H.code $ H.toHtml t
renderInline (PageMark t) = H.span ! A.class_ "pageMark" $ H.toHtml t
