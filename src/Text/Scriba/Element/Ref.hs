{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Ref where

import           Text.Scriba.Element.Identifier
import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Titling

import           Control.Monad.Except           ( MonadError(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

newtype SourceRef = SourceRef
  { getRef :: Identifier
  } deriving (Eq, Ord, Show, Read, Generic)

instance Numbering SourceRef
instance Titling i SourceRef

-- TODO: A resolved reference holds the identifier of a known element,
-- its type, its rendered number, its
data Ref a = Ref
  { refIdentifier :: Identifier
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

instance Numbering (Ref a)
instance Titling i (Ref a)

-- TODO: Not sure what to do here.
refToText :: (a -> [Text]) -> Ref a -> [Text]
refToText _ _ = []

pSourceRef :: Scriba Element SourceRef
pSourceRef = whileMatchTy "ref" $ do
  as <- meta $ args inspect
  case as of
    [t] -> useState [t] $ SourceRef <$> pIdent
    _   -> throwError $ Msg "ref takes exactly one identifier as an argument"

