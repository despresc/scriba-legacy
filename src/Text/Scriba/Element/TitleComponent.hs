{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Scriba.Element.TitleComponent where

import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Titling

import           Control.Applicative            ( (<|>) )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

-- TODO: it may be simpler to put all of this in Titling

data TitlePart
  = TitlePrefix
  | TitleNote
  | TitleNumber
  | TitleSep
  | TitleBody
  deriving (Eq, Ord, Show, Read, Generic, Numbering, Titling a)

-- A title component, with before and after components.
data TitleComponent i
  = TitleComponent TitlePart [i] [i] [i]
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering, Titling a)

titleComponentToText :: (i -> [Text]) -> TitleComponent i -> [Text]
titleComponentToText f (TitleComponent _ i j k) =
  concatMap f i <> concatMap f j <> concatMap f k

-- The fromTitleNumber doesn't need to have special markup associated
-- with it.
-- TODO: This might be changed to allow the i in TitleComponent i to
-- be different from the output i?
class FromTitleComponent i where
  fromTitleComponent :: TitleComponent i -> i
  fromTitleNumber :: Text -> i

-- TODO: better way of giving the components?
-- TODO: I could have both a titleNote and a titleBody. I suppose it
-- would go prefix, number, body, note, by default?
-- TODO: observe that this doesn't add the separator
-- TODO: somewhat inelegant just accepting a maybe template.
-- TODO: put the template style in the template itself?
runTemplate
  :: FromTitleComponent i
  => Maybe (TitleTemplate i)
  -> TitleTemplateStyle
  -> Maybe [i]
  -> Maybe [i]
  -> Maybe [i]
  -> Maybe [i]
runTemplate (Just template) ts tp tn tb =
  Just
    $  interpose sep
    $  mapMaybe mk
    $  condSwap (TitlePrefix, ttemplatePrefix template, tp)
                (TitleNumber, ttemplateNumber template, tn)
    <> [(fromTs, ttemplateBody template, tb)]
 where
  sep = ttemplatePartSep template
  condSwap x y = case ttemplatePrefixFirst template of
    True  -> [x, y]
    False -> [y, x]
  fromTs = case ts of
    FormalTemplate  -> TitleNote
    SectionTemplate -> TitleBody
  mk (p, Surround b def a, mcomp) =
    (mcomp <|> def) <&> \comp -> fromTitleComponent $ TitleComponent p b comp a
  interpose x (a : as) = a : beforeAll x as
  interpose _ []       = []
  beforeAll x (a : as) = x <> [a] <> beforeAll x as
  beforeAll _ []       = []
runTemplate Nothing _ _ _ _ = Nothing
