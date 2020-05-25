{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.WorkTitle where

import           Text.Scriba.Decorate
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

newtype WorkTitle i = WorkTitle
  { getWorkTitle :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)
    deriving anyclass (Numbering, Titling a, Gathering note)

instance Referencing a b => Referencing (WorkTitle a) (WorkTitle b)

workTitleToText :: (i -> [Text]) -> WorkTitle i -> [Text]
workTitleToText f (WorkTitle i) = concatMap f i

pWorkTitle :: Scriba Node a -> Scriba Element (WorkTitle a)
pWorkTitle = fmap WorkTitle . whileMatchTy "title" . allContentOf

instance RH.Render a => RH.Render (WorkTitle a) where
  render (WorkTitle i) =
    Html.span Html.! HtmlA.class_ "workTitle" <$> RH.render i
