{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Decorate.Common where

import           Text.Scriba.Counters

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype Identifier = Identifier
  { getIdentifier :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: artifically increase the relatedness of things in filtering?
-- We might want to say that list items (or other things) are
-- additionally related to particular containers for the purposes of
-- number rendering (useful for referencing, perhaps, if you want to
-- refer to "list item <secnum>.<subsecnum>.<itemnum>"). In that case
-- we would have to adjust DepthStyle (or add a related style) so that
-- only the depth of equal containers is used.
data ContainerPathFilter
  = FilterByCounterDep
  | FilterByContainer ContainerName
  deriving (Eq, Ord, Show, Read, Generic)

data LocalStyle
  = DepthStyle [LocalNumberStyle]
  | AbsoluteStyle LocalNumberStyle
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: distinction between display number and reference number? It's
-- not important right now, but the display number of a list is _only_
-- the local number, while the reference number is the full path.
data NumberStyle = NumberStyle
  { nsFilterMethod :: ContainerPathFilter
  , nsDisplayTake :: Maybe Int
  , nsStyles :: LocalStyle
  } deriving (Eq, Ord, Show, Read, Generic)

styleAtDepth :: Int -> [LocalNumberStyle] -> Maybe LocalNumberStyle
styleAtDepth _ [] = Nothing
styleAtDepth n (x : xs) | n > 0     = styleAtDepth (n - 1) xs
                        | otherwise = Just x

-- TODO: richer config for prefixing
data NumberConfig i = NumberConfig
  { ncNumberStyle :: NumberStyle
  , ncRefPrefix :: Maybe [i]
  , ncRefSep :: Maybe [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data UsedNumberConfig i = UsedNumberConfig
  { uncStyle :: LocalNumberStyle
  , uncRefPrefix :: Maybe [i]
  , uncRefSep :: Maybe [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

-- TODO: add more
data LocalNumberStyle
  = Decimal
  | LowerRoman
  | LowerAlpha
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

unzips :: Functor f => f (a, b) -> (f a, f b)
unzips x = (fmap fst x, fmap snd x)

unzips3 :: Functor f => f (a, b, c) -> (f a, f b, f c)
unzips3 x = (fmap fst' x, fmap snd' x, fmap thd' x)
 where
  fst' (a, _, _) = a
  snd' (_, b, _) = b
  thd' (_, _, c) = c
