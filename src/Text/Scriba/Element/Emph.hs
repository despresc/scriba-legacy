{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Emph where

import           Text.Scriba.Intermediate
import           Text.Scriba.Numbering

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


newtype Emph i = Emph
  { getEmph :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)
    deriving anyclass Numbering

emphToText :: (i -> [Text]) -> Emph i -> [Text]
emphToText f (Emph i) = concatMap f i

pEmph :: Scriba Node a -> Scriba Element (Emph a)
pEmph = fmap Emph . whileMatchTy "emph" . allContentOf

