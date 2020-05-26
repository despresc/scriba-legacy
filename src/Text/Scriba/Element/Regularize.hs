{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Regularize where

import           Text.Scriba.Decorate
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

-- TODO: what exactly should I do about numbering and regularization?
-- Currently regularize is transparent to numbering, but perhaps it
-- should disable it instead? Or we should add a bit of Numbering
-- state that disables automatic numbering.
data Regularize i = Regularize
  { regOld :: [i]
  , regNew :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)
    deriving anyclass (Numbering, Titling a)

instance Referencing a b => Referencing (Regularize a) (Regularize b)
instance Gathering note a b => Gathering note (Regularize a) (Regularize b)

regularizeToText :: (i -> [Text]) -> Regularize i -> [Text]
regularizeToText f (Regularize o n) = concatMap f o <> concatMap f n

pRegularize :: Scriba Node a -> Scriba Element (Regularize a)
pRegularize pInl = whileMatchTy "reg" $ meta $ attrs $ do
  old <- mattr "old" $ allContentOf pInl
  new <- mattr "new" $ allContentOf pInl
  pure $ Regularize old new

instance RH.Render a => RH.Render (Regularize a) where
  render (Regularize o n) = do
    o' <- RH.render o
    n' <- RH.render n
    pure $ Html.span Html.! HtmlA.class_ "reg" $ do
      Html.span Html.! HtmlA.class_ "old" $ o'
      Html.span Html.! HtmlA.class_ "new" $ n'
