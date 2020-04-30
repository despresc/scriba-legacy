{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Doc where

import           Text.Scriba.Counters
import           Text.Scriba.Element.Section
import           Text.Scriba.Element.TitleComponent
import           Text.Scriba.Intermediate
import           Text.Scriba.Numbering

import           Data.Functor                   ( (<&>) )
import qualified Data.List                     as List
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( mapMaybe )
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
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: should I mapKey the docNumberStyle here?
data DocAttrs i = DocAttrs
  { docTitle :: Title i
  , docPlainTitle :: Text
  , docTitlingConfig :: TitlingConfig i
  , docElemCounterRel :: Map ContainerName CounterName
  , docCounterRel :: Map CounterName (Set CounterName)
  , docNumberStyles :: Map Text NumberStyle
  } deriving (Eq, Ord, Show, Read, Generic)

data TitlingConfig i = TitlingConfig
  { tcFormalConfig :: Map Text (FormalConfig i)
  , tcSectionConfig :: Map Text (SectionConfig i)
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: richer whitespace options not in the body of the template?
-- E.g. stripping all whitespace, so that the template is a little
-- more understandable.
-- TODO: make the title template optional?
data FormalConfig i = FormalConfig
  { fconfTitleTemplate :: Maybe (TitleTemplate i)
  , fconfTitleSep :: Maybe [i]
  , fconfConcl :: Maybe [i]
  } deriving (Eq, Ord, Show, Read, Generic)

newtype SectionConfig i = SectionConfig
  { sconfTitleTemplate :: Maybe (TitleTemplate i)
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: have this be a common type, and have TitlePart use it? Would
-- need a middle bit that could be () to use in the title config. Or
-- perhaps not.
data Surround a = Surround
  { surroundBefore :: [a]
  , surroundMid :: Maybe [a]
  , surroundAfter :: [a]
  } deriving (Eq, Ord, Show, Read, Generic)

emptySurround :: Surround a
emptySurround = Surround [] Nothing []

-- TODO: might need more configuration related to component placement.
-- Perhaps also for whitespace?
-- TODO: I think a lot of these inline voids can be polymorphic, right?
data TitleTemplate a = TitleTemplate
  { ttemplatePrefix :: Surround a
  , ttemplateNumber :: Surround a
  , ttemplateBody :: Surround a
  , ttemplatePrefixFirst :: Bool
  } deriving (Eq, Ord, Show, Read, Generic)

data TitleTemplateStyle
  = FormalTemplate
  | SectionTemplate
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: better way of giving the components?
-- TODO: I could have both a titleNote and a titleBody. I suppose it
-- would go prefix, number, body, note, by default?
-- TODO: observe that this doesn't add the separator
-- TODO: somewhat inelegant just accepting a maybe template.
-- TODO: put the template style in the template itself?
runTemplate
  :: Maybe (TitleTemplate i)
  -> (TitleComponent i -> i)
  -> i
  -> TitleTemplateStyle
  -> Maybe [i]
  -> Maybe [i]
  -> Maybe [i]
  -> Maybe [i]
runTemplate (Just template) embed sep ts tp tn tb =
  Just
    $  List.intersperse sep
    $  mapMaybe mk
    $  condSwap (TitlePrefix, ttemplatePrefix template, tp)
                (TitleNumber, ttemplateNumber template, tn)
    <> [(fromTs, ttemplateBody template, tb)]
 where
  condSwap x y = case ttemplatePrefixFirst template of
    True  -> [x, y]
    False -> [y, x]
  fromTs = case ts of
    FormalTemplate  -> TitleNote
    SectionTemplate -> TitleBody
  mk (p, Surround b def a, mcomp) =
    (mcomp <|> def) <&> \comp -> embed $ TitleComponent p b comp a
runTemplate Nothing _ _ _ _ _ _ = Nothing

-- * Numbering

-- TODO: Doesn't number anything in the config. Should it?
numDoc :: Numbers [b i] -> Numbers [i] -> Numbers (Doc b i)
numDoc numBlocks numInls (Doc da f m b) = do
  f' <- numSectionContent numBlocks numInls f
  m' <- numSectionContent numBlocks numInls m
  b' <- numSectionContent numBlocks numInls b
  pure $ Doc da f' m' b'
