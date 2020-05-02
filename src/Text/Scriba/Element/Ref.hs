{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Ref where

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.Identifier
import           Text.Scriba.Intermediate

import           Control.Monad.Except           ( MonadError(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype SourceRef = SourceRef
  { sourceRefTarget :: Identifier
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering i SourceRef
instance Titling i SourceRef

-- TODO: may need more renditional information here, from Numbering and
-- Referencing, like relative position of the number and prefix.
-- May also need source overrides on certain elements of this.
data Ref i = Ref
  { refTarget :: Identifier
  , refContainer :: ContainerName
  , refNumberConfig :: NumberConfig i
  , refNumber :: Text
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

instance Numbering a i => Numbering a (Ref i)
instance Titling a i => Titling a (Ref i)
instance Referencing i a b => Referencing i (Ref a) (Ref b)

-- TODO: Not sure what to do here.
refToText :: (a -> [Text]) -> Ref a -> [Text]
refToText _ _ = []

pSourceRef :: Scriba Element SourceRef
pSourceRef = whileMatchTy "ref" $ do
  as <- meta $ args inspect
  case as of
    [t] -> useState [t] $ SourceRef <$> pIdent
    _   -> throwError $ Msg "ref takes exactly one identifier as an argument"

resolveRef :: SourceRef -> RefM i (Ref i)
resolveRef (SourceRef i) = do
  (cn, nc, num) <- lookupRefData i
  pure $ Ref i cn nc num
