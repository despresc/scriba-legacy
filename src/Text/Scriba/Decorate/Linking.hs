{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Text.Scriba.Decorate.Linking where

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Common

-- TODO: a common module for unzips and such?

import           Control.Monad.State.Strict     ( State
                                                , modify
                                                )
import qualified Control.Monad.State.Strict    as State
import           Data.Foldable                  ( traverse_ )
import           Data.Text                      ( Text )
import           Data.Void
import           GHC.Generics

{- TODO:

- may need to become aware of pagination
- may need to collect the document requirements here, too

-}

newtype LinkData = LinkData
  { getLinkData :: [LinkDatum]
  } deriving (Eq, Ord, Show, Semigroup, Monoid)

addLinkDatum :: LinkDatum -> LinkData -> LinkData
addLinkDatum x (LinkData y) = LinkData $ x : y

newtype LinkM a = LinkM
  { unNumberM :: State LinkData a
  } deriving (Functor, Applicative, Monad)

runLinkM :: LinkM a -> LinkData
runLinkM = ($ (LinkData [])) . State.execState . unNumberM

class GLinking f where
  glinking :: f a -> LinkM ()

instance GLinking U1 where
  glinking _ = pure ()

instance (GLinking a, GLinking b) => GLinking (a :*: b) where
  glinking (x :*: y) = glinking x >> glinking y

instance (GLinking a, GLinking b) => GLinking (a :+: b) where
  glinking (L1 x) = glinking x
  glinking (R1 y) = glinking y

instance GLinking a => GLinking (M1 j c a) where
  glinking (M1 x) = glinking x

instance Linking a => GLinking (K1 j a) where
  glinking (K1 x) = linking x

class Linking a where
  linking :: a -> LinkM ()

  default linking :: (Generic a, GLinking (Rep a)) => a -> LinkM ()
  linking = glinking . from

instance Linking a => Linking (Maybe a) where
  linking = traverse_ linking

instance Linking a => Linking [a] where
  linking = traverse_ linking

instance Linking Text where
  linking _ = pure ()
instance Linking Identifier
instance Linking ElemNumber
instance Linking Bool
instance Linking ContainerName
instance Linking UsedNumberConfig where
  linking _ = pure ()
instance Linking Void where
  linking = absurd

tellLinkDatum :: LinkDatum -> LinkM ()
tellLinkDatum = LinkM . modify . addLinkDatum

tellLinkNumbered :: Text -> Maybe Identifier -> Maybe ElemNumber -> LinkM ()
tellLinkNumbered t mi (Just en) = tellLinkDatum $ LinkNumber mi t en
tellLinkNumbered t mi _         = tellLinkGen t mi

tellLinkGen :: Text -> Maybe Identifier -> LinkM ()
tellLinkGen t = traverse_ $ tellLinkDatum . flip LinkBare t
