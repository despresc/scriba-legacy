{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Scriba.Element.Metadata where

import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.DocAttrs
import           Text.Scriba.Element.Identifier ( pIdent )
import           Text.Scriba.Element.TitleComponent
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Monad                  ( join )
import           Control.Monad.Reader           ( asks )
import           Control.Monad.State            ( gets )
import           Data.Functor                   ( (<&>) )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

{-

Metadata:

- original authorship and such (perhaps collected from the sources?)

- sources of the work (for digitized things). Possibly designating one
  or more sources as primary?

- editors of the digitization?

We should just collect all the sources together and display them as a
single citation, perhaps, in a summary. (So author: Riemann, editor:
whoever1, whoever2, where whoever1 and 2 are potentially from
different editions)

-}

data DigitalSources = DigitialSources
