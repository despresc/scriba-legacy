{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Text.Scriba.Decorate.Gathering where

import           Text.Scriba.Counters
import           Text.Scriba.Decorate.Common

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Except           ( Except
                                                , runExcept
                                                , liftEither
                                                )
import           Control.Monad.State.Strict     ( StateT
                                                , MonadState(..)
                                                , modify
                                                )
import qualified Control.Monad.State.Strict    as State
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import           Data.Void
import           GHC.Generics

{- TODO:

- may need to become aware of pagination
- may need to collect the document requirements here, too

-}

data GatherData note = GatherData
  { gatherLinkData :: Map Identifier LinkDatum
  , gatherNoteText :: Map Identifier note
  } deriving (Eq, Ord, Show)

instance Semigroup (GatherData note) where
  (GatherData l n) <> (GatherData l' n') = GatherData (l <> l') (n <> n')

instance Monoid (GatherData note) where
  mempty = GatherData mempty mempty

addLinkDatum
  :: Identifier
  -> LinkDatum
  -> Map Identifier LinkDatum
  -> Either DecorateError (Map Identifier LinkDatum)
addLinkDatum = insertUnique $ \(Identifier i) _ ->
  DecorateError $ "identifier <" <> i <> "> was defined twice in the document"

addNoteText :: Identifier -> note -> GatherData note -> GatherData note
addNoteText i n (GatherData l m) = GatherData l $ Map.insert i n m

newtype GatherM note a = GatherM
  { unNumberM :: StateT (GatherData note) (Except DecorateError) a
  } deriving (Functor, Applicative, Monad)

runGatherM :: GatherM note a -> Either DecorateError (a, GatherData note)
runGatherM = runExcept . ($ mempty) . State.runStateT . unNumberM

class GGathering note f g where
  ggathering :: f a -> GatherM note (g a)

instance GGathering note U1 U1 where
  ggathering = pure

instance (GGathering note a b, GGathering note c d) => GGathering note (a :*: c) (b :*: d) where
  ggathering (x :*: y) = liftA2 (:*:) (ggathering x) (ggathering y)

instance (GGathering note a b, GGathering note c d) => GGathering note (a :+: c) (b :+: d) where
  ggathering (L1 x) = L1 <$> ggathering x
  ggathering (R1 y) = R1 <$> ggathering y

instance GGathering note a b => GGathering note (M1 j c a) (M1 j c b) where
  ggathering (M1 x) = M1 <$> ggathering x

instance Gathering note a b => GGathering note (K1 j a) (K1 j b) where
  ggathering (K1 x) = K1 <$> gathering x

class Gathering note a b where
  gathering :: a -> GatherM note b

  default gathering :: (Generic a, Generic b, GGathering note (Rep a) (Rep b))
                      => a -> GatherM note b
  gathering = fmap to . ggathering . from

instance Gathering note a b => Gathering note (Maybe a) (Maybe b) where
  gathering = traverse gathering

instance Gathering note a b => Gathering note [a] [b] where
  gathering = traverse gathering

instance Gathering note Text Text where
  gathering = pure
instance Gathering note Identifier Identifier
instance Gathering note ElemNumber ElemNumber
instance Gathering note Bool Bool
instance Gathering note Int Int where
  gathering = pure
instance Gathering note NumberAuto NumberAuto
instance Gathering note ContainerName ContainerName
instance Gathering note UsedNumberConfig UsedNumberConfig where
  gathering = pure
instance Gathering note Void a where
  gathering = absurd
instance Gathering note (Void1 a) b where
  gathering = absurd1
instance Gathering note () ()

-- TODO: should this be moved elsewhere?
class HasNil a where
  embedNil :: a

tellLinkDatum :: Maybe Identifier -> LinkDatum -> GatherM note ()
tellLinkDatum (Just i) ld = GatherM $ do
  GatherData lds nt <- get
  lds'              <- liftEither $ addLinkDatum i ld lds
  put $ GatherData lds' nt
tellLinkDatum Nothing _ = pure ()

tellLinkNumbered
  :: Text -> Maybe Identifier -> Maybe ElemNumber -> GatherM note ()
tellLinkNumbered t mi (Just en) = tellLinkDatum mi $ LinkNumber t en
tellLinkNumbered t mi _         = tellLinkGen t mi

tellLinkGen :: Text -> Maybe Identifier -> GatherM note ()
tellLinkGen t = flip tellLinkDatum $ LinkBare t

-- TODO: Note that we don't perform double-checking of note text
-- identifiers here, since that is presently handled by
-- tellLinkDatum. Perhaps that should change.
tellNoteText :: Identifier -> note -> GatherM note ()
tellNoteText i = GatherM . modify . addNoteText i
