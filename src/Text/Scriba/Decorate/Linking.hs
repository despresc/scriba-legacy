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

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( join )
import           Control.Monad.Except           ( Except
                                                , runExcept
                                                , MonadError(..)
                                                )
import           Control.Monad.State.Strict     ( State
                                                , MonadState(..)
                                                , gets
                                                , modify
                                                )
import qualified Control.Monad.State.Strict    as State
import           Data.Foldable                  ( traverse_
                                                , for_
                                                )
import           Data.Digits                    ( digits )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                , catMaybes
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Traversable               ( for )
import           Data.Void
import           GHC.Generics
import           Text.Numeral.Roman

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

tellDatum :: LinkDatum -> LinkM ()
tellDatum = LinkM . modify . addLinkDatum
