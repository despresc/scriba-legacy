{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Text.Scriba.Titling where

import           Control.Applicative            ( liftA2 )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , Reader
                                                , runReader
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import           Data.Void
import           GHC.Generics

-- TODO: Note that much of the concrete titling logic happens in
-- Element.TitleComponent. May want to improve the organization.

-- TODO: this also responsible for generating the conclusion of formal
-- blocks. Sort of misleading that it happens here, perhaps...

-- TODO: richer whitespace options not in the body of the template?
-- E.g. stripping all whitespace, so that the template is a little
-- more understandable.
-- TODO: make the title template optional?
data FormalConfig i = FormalConfig
  { fconfTitleTemplate :: Maybe (TitleTemplate i)
  , fconfTitleSep :: Maybe [i]
  , fconfConcl :: Maybe [i]
  } deriving (Eq, Ord, Show, Read, Generic)

newtype SectionConfig i = SectionConfig
  { sconfTitleTemplate :: Maybe (TitleTemplate i)
  } deriving (Eq, Ord, Show, Read, Generic)

-- Consolidate the title templates?
data TitlingConfig i = TitlingConfig
  { tcFormalConfig :: Map Text (FormalConfig i)
  , tcSectionConfig :: Map Text (SectionConfig i)
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: might need more configuration related to component placement.
-- Perhaps also for whitespace?
-- TODO: I think a lot of these inline voids can be polymorphic, right?
data TitleTemplate a = TitleTemplate
  { ttemplatePrefix :: Surround a
  , ttemplateNumber :: Surround a
  , ttemplateBody :: Surround a
  , ttemplatePartSep :: [a]
  , ttemplatePrefixFirst :: Bool
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: have this be a common type, and have TitlePart use it? Would
-- need a middle bit that could be () to use in the title config. Or
-- perhaps not.
data Surround a = Surround
  { surroundBefore :: [a]
  , surroundMid :: Maybe [a]
  , surroundAfter :: [a]
  } deriving (Eq, Ord, Show, Read, Generic)

newtype TitleM i a = TitleM
  { unTitleM :: Reader (TitlingConfig i) a
  } deriving (Functor, Applicative, Monad, MonadReader (TitlingConfig i))

runTitleM :: TitleM i a -> TitlingConfig i -> a
runTitleM = runReader . unTitleM

class GTitling i f where
  gtitling :: f a -> TitleM i (f a)

instance GTitling i U1 where
  gtitling = pure

instance (GTitling i a, GTitling i b) => GTitling i (a :*: b) where
  gtitling (x :*: y) = liftA2 (:*:) (gtitling x) (gtitling y)

instance (GTitling i a, GTitling i b) => GTitling i (a :+: b) where
  gtitling (L1 x) = L1 <$> gtitling x
  gtitling (R1 y) = R1 <$> gtitling y

instance GTitling i a => GTitling i (M1 j c a) where
  gtitling (M1 x) = M1 <$> gtitling x

instance Titling i a => GTitling i (K1 j a) where
  gtitling (K1 x) = K1 <$> titling x

-- TODO: can we reduce duplication with Numbering?

-- TODO: find a way to improve the signatures of things. The (Titling
-- (Inline a) (Inline a), Titling (Inline a) (Block (Inline a))) stuff
-- is a little much. Could just add a constraint synonym, perhaps.
class Titling i a where
  titling :: a -> TitleM i a

  default titling :: (Generic a, GTitling i (Rep a)) => a -> TitleM i a
  titling = fmap to . gtitling . from

instance Titling i a => Titling i [a] where
  titling = traverse titling

instance Titling i a => Titling i (Maybe a) where
  titling = traverse titling

instance Titling i () where
  titling = pure

instance Titling i Bool where
  titling = pure

instance Titling i Void where
  titling = absurd

instance Titling i Text where
  titling = pure

instance Titling i a => Titling i (Map k a) where
  titling = M.traverseWithKey $ const titling

data TitleTemplateStyle
  = FormalTemplate
  | SectionTemplate
  deriving (Eq, Ord, Show, Read, Generic)
