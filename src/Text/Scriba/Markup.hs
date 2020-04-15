{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Markup where

import qualified Text.Scriba.Intermediate      as I

import           Control.Monad.Except           ( Except
                                                , MonadError(..)
                                                )
import qualified Control.Monad.Except          as E
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

{- TODO:

- Actually finish this

- Better errors everywhere (use source positions, for one thing)

-}

-- TODO: section types, titles, other things...
data Section = Section
  { secPreamble :: [Block]
  , secChildren :: [Section]
  } deriving (Eq, Ord, Show, Read)

data Block
  = Paragraph [ParaContent]
  deriving (Eq, Ord, Show, Read)

data ParaContent
 -- TODO: Paragraphs shouldn't contain paragraphs, of course.
  = ParBlock Block
  | ParInline Inline
  deriving (Eq, Ord, Show, Read)

data Inline
  = Str Text
  | Emph [Inline]
  -- TODO: page mark should be some kind of locator?
  | PageMark Text
  deriving (Eq, Ord, Show, Read)

-- TODO: perhaps move this to its own module?

-- | Simple error monad.
newtype ScribaM a = ScribaM
  { getScribaM :: Except Text a
  } deriving (Functor, Applicative, Monad, MonadError Text)

runScribaM :: ScribaM a -> Either Text a
runScribaM = E.runExcept . getScribaM
