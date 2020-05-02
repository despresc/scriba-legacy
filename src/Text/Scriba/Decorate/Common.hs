{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Decorate.Common where

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype Identifier = Identifier
  { getIdentifier :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: richer config for prefixing
data NumberConfig i = NumberConfig
  { ncNumberStyle :: NumberStyle
  , ncRefPrefix :: Maybe [i]
  , ncRefSep :: Maybe [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data NumberStyle = Decimal
  deriving (Eq, Ord, Show, Read, Generic)

-- Consolidate the title templates?
data TitlingConfig i = TitlingConfig
  { tcFormalConfig :: Map Text (FormalConfig i)
  , tcSectionConfig :: Map Text (SectionConfig i)
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

-- TODO: richer whitespace options not in the body of the template?
-- E.g. stripping all whitespace, so that the template is a little
-- more understandable.
-- TODO: make the title template optional?
data FormalConfig i = FormalConfig
  { fconfTitleTemplate :: Maybe (TitleTemplate i)
  , fconfTitleSep :: Maybe [i]
  , fconfConcl :: Maybe [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

newtype SectionConfig i = SectionConfig
  { sconfTitleTemplate :: Maybe (TitleTemplate i)
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

-- TODO: might need more configuration related to component placement.
-- Perhaps also for whitespace?
-- TODO: I think a lot of these inline voids can be polymorphic, right?
data TitleTemplate a = TitleTemplate
  { ttemplatePrefix :: Surround a
  , ttemplateNumber :: Surround a
  , ttemplateBody :: Surround a
  , ttemplatePartSep :: [a]
  , ttemplatePrefixFirst :: Bool
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

-- TODO: have this be a common type, and have TitlePart use it? Would
-- need a middle bit that could be () to use in the title config. Or
-- perhaps not.
data Surround a = Surround
  { surroundBefore :: [a]
  , surroundMid :: Maybe [a]
  , surroundAfter :: [a]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)


-- TODO: make this richer. Will need source positions for things in
-- the markup to make these errors better.
data DecorateError
  = DecorateError Text
  | DecorateNil
  deriving (Eq, Ord, Show)

instance Semigroup DecorateError where
  x@DecorateError{} <> _ = x
  DecorateNil       <> y = y

instance Monoid DecorateError where
  mempty = DecorateNil
