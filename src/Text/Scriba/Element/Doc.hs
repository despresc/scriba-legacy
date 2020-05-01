{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Doc where

import           Text.Scriba.Counters
import           Text.Scriba.Element.Section
import           Text.Scriba.Element.TitleComponent
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Titling

import           Data.Map.Strict                ( Map )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

{- TODO:

- split this further. The titling config and numbering config should
  be bundled and put in a separate module.

- improve runTemplate

-}

-- | A document with front matter, main matter, and end matter.

data Doc b i = Doc (DocAttrs i) (SectionContent b i) (SectionContent b i) (SectionContent b i)
  deriving (Eq, Ord, Show, Read, Generic, Functor)

-- TODO: should I mapKey the docNumberStyle here?
data DocAttrs i = DocAttrs
  { docTitle :: Title i
  , docPlainTitle :: Text
  , docTitlingConfig :: TitlingConfig i
  , docElemCounterRel :: Map ContainerName CounterName
  , docCounterRel :: Map CounterName (Set CounterName)
  , docNumberStyles :: Map Text NumberStyle
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

emptySurround :: Surround a
emptySurround = Surround [] Nothing []

-- * Numbering

-- TODO: Doesn't number anything in the config. Should it?
instance (Numbering (b i), Numbering i) => Numbering (Doc b i) where
  numbering (Doc da f m b) = do
    f' <- numbering f
    m' <- numbering m
    b' <- numbering b
    pure $ Doc da f' m' b'

instance (Titling i (b i), Titling i i, FromTitleComponent i) => Titling i (Doc b i) where
  titling (Doc da f m b) = do
    f' <- titling f
    m' <- titling m
    b' <- titling b
    pure $ Doc da f' m' b'
