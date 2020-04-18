{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Scriba.Markup where

import           Text.Scriba.Intermediate

import           Control.Applicative            ( (<|>)
                                                , Alternative
                                                )
import           Control.Monad                  ( MonadPlus )
import           Control.Monad.Except           ( Except
                                                , MonadError(..)
                                                )
import qualified Control.Monad.Except          as E
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

{- TODO:

- Actually finish this

- Better errors everywhere (use source positions, for one thing)

- Add blocks (figure out what to do about them and paragraphs?), more
  inlines, sections.

-}

-- | A document with front matter, main matter, and end matter.
data Doc = Doc [Paragraph] [Paragraph] [Paragraph]
  deriving (Eq, Ord, Show, Read)

data Paragraph = Paragraph [ParContent]
  deriving (Eq, Ord, Show, Read)

data ParContent
  = ParInline Inline
  deriving (Eq, Ord, Show, Read)

data Inline
  = Str Text
  | Emph [Inline]
  | Math Text
  -- TODO: page mark should be some kind of locator?
  | PageMark Text
  deriving (Eq, Ord, Show, Read)

-- TODO: move this to its own module?

-- | Simple error monad.

newtype Scriba a = Scriba
  { getScriba :: Except ScribaError a
  } deriving (Functor, Applicative, Monad, MonadError ScribaError, Alternative, MonadPlus)

runScriba :: Scriba a -> Either ScribaError a
runScriba = E.runExcept . getScriba

-- TODO: expectation definitions and accumulation?
data ScribaError
  = Error Text
  | ErrorNil
  deriving (Eq, Ord, Show, Read)

instance Semigroup ScribaError where
  (Error t) <> _ = Error t
  _         <> e = e

instance Monoid ScribaError where
  mempty = ErrorNil

throwScribaError :: MonadError ScribaError m => Text -> m a
throwScribaError = throwError . Error
-- TODO: develop a better tree parsing infrastructure. I have some
-- work along those lines in old/alcuin-ml and in
-- pandoc-metaparsing. Essentially, something similar to what
-- aeson/(mega)parsec does, CPS with state. Certainly want some way of
-- annotating these things with expectations, so we can have
-- "expecting ..." in our errors.

-- TODO: not very good. Doesn't report expectations.
asNode :: (Element -> Scriba a) -> Node -> Scriba a
asNode p (NodeElement e) = p e
asNode _ (NodeText sp _) =
  throwScribaError
    $  "Text encountered at "
    <> T.pack (show sp)
    <> " when an element was expected"

pParagraph :: Element -> Scriba Paragraph
pParagraph (Element (Just "p") _ n) = Paragraph <$> traverse pParContent n
pParagraph _                        = throwScribaError "no paragraph parse"

pParContent :: Node -> Scriba ParContent
pParContent n = ParInline <$> pInline n

pInline :: Node -> Scriba Inline
pInline n =
  asNode pEmph n <|> asNode pPageMark n <|> asNode pMath n <|> pText n

pEmph :: Element -> Scriba Inline
pEmph (Element (Just "emph") _ n) = Emph <$> traverse pInline n
pEmph _                           = throwScribaError "no emph parse"

-- TODO: well-formedness checking?
pPageMark :: Element -> Scriba Inline
pPageMark (Element (Just "physPage") _ [NodeText _ t]) = pure $ PageMark t
pPageMark _ = throwScribaError "no physPage parse"

pText :: Node -> Scriba Inline
pText (NodeText _ t) = pure $ Str t
pText _              = throwScribaError "no text parse"

pMath :: Element -> Scriba Inline
pMath (Element (Just "math") _ n) = Math . T.concat <$> traverse pJustText n
 where
  pJustText (NodeText _ t) = pure t
  pJustText _ = throwScribaError "markup not allowed in a math element"
pMath _ = throwScribaError "no math parse"

-- * Document parsing

pDoc :: Element -> Scriba Doc
pDoc (Element (Just "scriba") _ n) = pExplicitMatter n <|> pBare n
 where
  -- TODO: this really shows the need for more advanced parsing
  -- facilities!
  pMatter ty (Element (Just t) _ nodes) | ty == t =
    traverse (asNode pParagraph) nodes
  pMatter ty _ = throwScribaError $ "no " <> ty <> " parse"
  pExplicitMatter [f, m, e] = do
    f' <- asNode (pMatter "frontMatter") f
    m' <- asNode (pMatter "mainMatter") m
    e' <- asNode (pMatter "backMatter") e
    pure $ Doc f' m' e'
  pExplicitMatter _ = throwScribaError "no explicit matter parse"
  pBare nodes = do
    nodes' <- traverse (asNode pParagraph) nodes
    pure $ Doc [] nodes' []
pDoc _ = throwScribaError "no scriba parse"

-- * Running parsers

parseDoc :: Node -> Either ScribaError Doc
parseDoc = runScriba . asNode pDoc
