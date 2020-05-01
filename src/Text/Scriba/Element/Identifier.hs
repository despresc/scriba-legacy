{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Identifier
  ( Identifier(..)
  , pIdent
  )
where

import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Common

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Char                      ( isSpace )
import qualified Data.Text                     as T

-- TODO: not sure I want this here. Could just put it in Linking...

-- Parse identifier content without whitespace
-- TODO: some kind of position update fold would be nice here.
pIdent :: Scriba [Node] Identifier
pIdent = do
  t <- T.concat <$> remaining simpleText
  let t' = T.strip t
  when (T.null t') $ throwError $ Msg "identifier must be non-empty"
  when (T.any isSpace t') $ throwError $ Msg
    "identifier cannot have whitespace in it"
  pure $ Identifier t'
