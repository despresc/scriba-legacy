{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Emph where

import           Text.Scriba.Decorate
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html

newtype Emph i = Emph
  { getEmph :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)
    deriving anyclass (Numbering, Titling a)

instance Referencing a b => Referencing (Emph a) (Emph b)
instance Gathering note a b => Gathering note (Emph a) (Emph b)

emphToText :: (i -> [Text]) -> Emph i -> [Text]
emphToText f (Emph i) = concatMap f i

pEmph :: Scriba Node a -> Scriba Element (Emph a)
pEmph = fmap Emph . whileMatchTy "emph" . allContentOf

instance RH.Render a => RH.Render (Emph a) where
  render (Emph i) = Html.em <$> RH.render i
