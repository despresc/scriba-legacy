{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.TitleComponent where

import           Text.Scriba.Decorate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Applicative            ( (<|>) )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

-- TODO: it may be simpler to put all of this in Titling

data TitlePart
  = TitlePrefix
  | TitleNote
  | TitleNumber
  | TitleSep
  | TitleBody
  deriving (Eq, Ord, Show, Read, Generic, Numbering, Titling a, Linking)

instance Referencing TitlePart TitlePart

-- A title component, with before and after components.
data TitleComponent i
  = TitleComponent TitlePart [i] [i] [i]
  deriving (Eq, Ord, Show, Read, Functor, Generic, Numbering, Titling a, Linking)

instance Referencing a b => Referencing (TitleComponent a) (TitleComponent b)

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
  condSwap x y = if ttemplatePrefixFirst template then [x, y] else [y, x]
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

instance RH.Render i => RH.Render (TitleComponent i) where
  render (TitleComponent t a v b) =
    Html.span Html.! HtmlA.class_ wrapClass <$> body
   where
    wrapClass = case t of
      TitlePrefix -> "titlePrefix"
      TitleNote   -> "titleNote"
      TitleNumber -> "number"
      TitleSep    -> "titleSep"
      TitleBody   -> "titleBody"
    body = do
      a' <- RH.render a
      v' <- RH.render v
      b' <- RH.render b
      pure $ do
        Html.span Html.! HtmlA.class_ "before" $ a'
        v'
        Html.span Html.! HtmlA.class_ "after" $ b'
