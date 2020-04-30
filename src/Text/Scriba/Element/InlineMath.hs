{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.InlineMath where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Text.Scriba.Intermediate

newtype InlineMath = InlineMath
  { getInlineMath :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

inlineMathToText :: InlineMath -> [Text]
inlineMathToText (InlineMath t) = [t]

pMath :: Scriba Element InlineMath
pMath = InlineMath . T.concat <$> whileMatchTy "math" (allContentOf simpleText)

