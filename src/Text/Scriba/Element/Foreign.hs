{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Foreign where

import           Text.Scriba.Decorate
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

data Foreign i = Foreign
  { foreignLang :: Maybe Text
  , foreignBody :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor, Numbering, Titling a)

instance Referencing a b => Referencing (Foreign a) (Foreign b)
instance Gathering note a b => Gathering note (Foreign a) (Foreign b)

instance RH.Render i => RH.Render (Foreign i) where
  render (Foreign ml t) = do
    t' <- RH.render t
    let mlang = HtmlA.lang . Html.toValue <$> ml
    pure $ Html.span RH.?? mlang Html.! HtmlA.class_ "foreign" $ t'

pForeign :: Scriba Node a -> Scriba Element (Foreign a)
pForeign pInl = whileMatchTy "foreign" $ do
  mi <- meta $ attrs $ attrMaybe "lang" $ T.concat <$> allContentOf simpleText
  c  <- allContentOf pInl
  pure $ Foreign mi c

foreignToText :: (i -> [Text]) -> Foreign i -> [Text]
foreignToText f (Foreign _ i) = concatMap f i
