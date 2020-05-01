{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Paragraph where

import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Titling

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype Paragraph i = Paragraph
  { getParagraph :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)
    deriving anyclass (Numbering, Titling a)

paragraphToText :: (i -> [Text]) -> Paragraph i -> [Text]
paragraphToText f (Paragraph t) = concatMap f t

pParagraph :: Scriba [Node] [i] -> Scriba Element (Paragraph i)
pParagraph p = do
  etp <- eitherP (matchTy "p") presentedAsParagraph
  let e = case etp of
        Left  _ -> "p"
        Right _ -> "paragraph block"
  c <- whileParsingElem e $ content p
  pure $ Paragraph c
 where
  presentedAsParagraph = do
    meta $ do
      Meta _ pres _ _ <- inspect
      case pres of
        AsPara -> pure ()
        _      -> empty
