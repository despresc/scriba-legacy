{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Emph where

import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


newtype Emph i = Emph
  { getEmph :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)
    deriving anyclass (Numbering a, Titling a)

instance Referencing i a b => Referencing i (Emph a) (Emph b)

emphToText :: (i -> [Text]) -> Emph i -> [Text]
emphToText f (Emph i) = concatMap f i

pEmph :: Scriba Node a -> Scriba Element (Emph a)
pEmph = fmap Emph . whileMatchTy "emph" . allContentOf

