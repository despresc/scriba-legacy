{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Decorate.Common where

import           Text.Scriba.Counters

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )

data Void1 a
  deriving (Eq, Ord, Show, Read)

absurd1 :: Void1 a -> b
absurd1 = \case {}

insertUnique
  :: Ord k => (k -> v -> e) -> k -> v -> Map k v -> Either e (Map k v)
insertUnique f k v = Map.alterF go k
 where
  go Nothing = Right $ Just v
  go Just{}  = Left $ f k v

newtype Identifier = Identifier
  { getIdentifier :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

newtype LibUrlPart = LibUrlPart Text
  deriving (Eq, Ord, Show, Read, Generic, IsString)

urlPartToIdentifier :: LibUrlPart -> Identifier
urlPartToIdentifier (LibUrlPart t) = Identifier t

renderLibUrlPart :: LibUrlPart -> Text
renderLibUrlPart (LibUrlPart t) = t

newtype LibDomain = LibDomain Text
  deriving (Eq, Ord, Show, Read, Generic, IsString)

renderLibDomain :: LibDomain -> Text
renderLibDomain (LibDomain t) = t

-- No support for queries or versions yet
-- TODO: should the [LibUrlPart] be in reverse order?
data LibUrl = LibUrl
  { libUrlDomain :: Maybe LibDomain
  , libUrlPath :: [LibUrlPart]
  } deriving (Eq, Ord, Show, Read, Generic)

appendToUrl :: LibUrl -> LibUrlPart -> LibUrl
appendToUrl (LibUrl d p) part = LibUrl d (p <> [part])

-- TODO: get rid of RefTarget entirely!
libUrlToRefTarget :: LibUrl -> LibUrlPart -> LibUrl -> Maybe RefTarget
libUrlToRefTarget u i u' = case u `prefixOfLibUrl` u' of
  Just [p, q]
    | p == i -> Just $ RefSelf (urlPartToIdentifier q)
    | otherwise -> Just
    $  RefQualified (urlPartToIdentifier p) (urlPartToIdentifier q)
  _ -> Nothing

renderLibUrl :: LibUrl -> Text
renderLibUrl (LibUrl md ps) = "library:/"
  <> Text.intercalate "/" (p <> (renderLibUrlPart <$> ps))
 where
  p = case md of
    Nothing -> []
    Just d  -> ["/" <> renderLibDomain d]

-- Returns Just ps if the first is a prefix of the second
prefixOfLibUrl :: LibUrl -> LibUrl -> Maybe [LibUrlPart]
prefixOfLibUrl (LibUrl d p) (LibUrl d' p') | d == d'   = go p p'
                                           | otherwise = Nothing
 where
  go (x : xs) (y : ys) | x == y = go xs ys
  go [] ys                      = Just ys
  go _  _                       = Nothing

data RefTarget
  = RefSelf Identifier
  | RefQualified Identifier Identifier
  deriving (Eq, Ord, Show, Read, Generic)

refTargetPretty :: RefTarget -> Text
refTargetPretty (RefSelf (Identifier i)                    ) = i
refTargetPretty (RefQualified (Identifier i) (Identifier j)) = i <> "." <> j

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
data NumberConfig = NumberConfig
  { ncNumberStyle :: NumberStyle
  , ncRefPrefix :: Maybe Text
  , ncRefSep :: Maybe Text
  } deriving (Eq, Ord, Show, Read, Generic)

data UsedNumberConfig = UsedNumberConfig
  { uncStyle :: LocalNumberStyle
  , uncRefPrefix :: Maybe Text
  , uncRefSep :: Maybe Text
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: better name?
data ElemNumber
  = ElemNumberAuto NumberAuto
  | NumberSource Text
  deriving (Eq, Ord, Show, Read, Generic)

data NumberAuto = NumberAuto ContainerName UsedNumberConfig Int Text
  deriving (Eq, Ord, Show, Read, Generic)

elemNumberNum :: ElemNumber -> Text
elemNumberNum (ElemNumberAuto (NumberAuto _ _ _ t)) = t
elemNumberNum (NumberSource   t                   ) = t

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

-- The Text is the reference prefix that ought to be used for the
-- identifier. Only useful for mathjax equations, and that might be
-- changing anyway.
data LinkDatum
  = LinkNumber Text PageName ElemNumber
  | LinkBare Text PageName
  deriving (Eq, Ord, Show, Read, Generic)

linkDatumPrefix :: LinkDatum -> Text
linkDatumPrefix (LinkNumber t _ _) = t
linkDatumPrefix (LinkBare t _    ) = t

-- | Numbering data to be gathered from the AST.
data NumberDatum = NumberDatum
  { ndIdentifier :: Identifier
  , ndContainerName :: ContainerName
  , ndNumberConfig :: UsedNumberConfig
  , ndNumber :: Text
  } deriving (Eq, Ord, Show)

-- TODO: make this richer. Will need source positions for things in
-- the markup to make these errors better.
data DecorateError
  = DecorateError Text
  | DecorateNil
  deriving (Eq, Ord, Show)

prettyDecorateError :: DecorateError -> Text
prettyDecorateError (DecorateError t) = t
prettyDecorateError DecorateNil       = ""

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

newtype PageName = PageName Text
  deriving (Eq, Ord, Show, Read, Generic)
