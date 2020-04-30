{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Emph where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Text.Scriba.Intermediate

newtype Emph i = Emph
  { getEmph :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

emphToText :: (i -> [Text]) -> Emph i -> [Text]
emphToText f (Emph i) = concatMap f i

pEmph :: Scriba Node a -> Scriba Element (Emph a)
pEmph = fmap Emph . whileMatchTy "emph" . allContentOf

