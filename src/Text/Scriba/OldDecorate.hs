{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Scriba.OldDecorate
  ( decorate
  )
where

-- Decorate the internal representation in various ways.

import           Text.Scriba.Markup
import           Text.Scriba.Numbering

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( join )
import           Data.Functor                   ( ($>) )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import           Data.Void

-- TODO: may need errors, a state for numbering, environment for
-- titling. Or perhaps pipelined for modularity, with a shared error
-- type?
decorate :: Doc Block (Inline Void) -> Doc Block (Inline Void)
decorate = genDocTitle . runNumDoc

data DecorateError
  = DecorateError Text
  | DecorateNil
  deriving (Eq, Ord, Show, Read)

mergeDecorateError :: DecorateError -> DecorateError -> DecorateError
mergeDecorateError x@DecorateError{} _                 = x
mergeDecorateError _                 y@DecorateError{} = y
mergeDecorateError DecorateNil       DecorateNil       = DecorateNil

instance Semigroup DecorateError where
  (<>) = mergeDecorateError

instance Monoid DecorateError where
  mempty = DecorateNil

-- * Numbering

defaultNumberState :: DocAttrs i -> NumberState
defaultNumberState da = NumberState initCounters
                                    []
                                    (docNumberStyles da)
                                    (docCounterRel da)
                                    (docElemCounterRel da)
  where initCounters = docCounterRel da $> 1

runNumDoc :: Numbering a => Doc Block (Inline a) -> Doc Block (Inline a)
runNumDoc d@(Doc da _ _ _) =
  flip runNumberM (defaultNumberState da) $ numbering d

-- * Generating titles

genDocTitle :: Doc Block (Inline Void) -> Doc Block (Inline Void)
genDocTitle (Doc met f m b) = Doc met (go f) (go m) (go b)
 where
  dfc = docTitlingConfig met
  go  = genSecContentTitle dfc

-- TODO: Make a Reader for this stuff

-- TODO: really shows that a Walkable class is necessary.
genSecContentTitle
  :: TitlingConfig (Inline Void)
  -> SectionContent Block (Inline Void)
  -> SectionContent Block (Inline Void)
genSecContentTitle m (SectionContent p c) =
  SectionContent (genBlockTitle m <$> p) (genSectionTitle m <$> c)

-- TODO: we don't walk any inlines because there is nothing to
-- generate for them. That might change!
genBlockTitle
  :: TitlingConfig (Inline Void) -> Block (Inline Void) -> Block (Inline Void)
genBlockTitle m (Bformal formal) = Bformal $ genFormalTitle m formal
genBlockTitle m (Blist   l     ) = Blist $ genListTitle m l
genBlockTitle _ x                = x

-- TODO: this also generates the conclusion of formal blocks. Sort of
-- misleading that it happens here, perhaps...
genFormalTitle
  :: TitlingConfig (Inline Void)
  -> Formal Block (Inline Void)
  -> Formal Block (Inline Void)
genFormalTitle m (Formal mty mnum mti mnote mtisep cont conc) = Formal
  mty
  mnum
  (mti <|> join mtigen)
  mnote
  (mtisep <|> join mtisep')
  (genMixedBlockBodyTitle m cont)
  (conc <|> join concgen)
 where
  (mtisep', mtigen, concgen) = unzips3 $ do
    t     <- mty
    fconf <- M.lookup t $ tcFormalConfig m
    let concl    = fconfConcl fconf
        tisep    = fconfTitleSep fconf
        template = fconfTitleTemplate fconf
        toInlStr = (: []) . Istr . Str
    pure
      ( tisep
      , runTemplate template
                    ItitleComponent
                    (Istr $ Str " ")
                    FormalTemplate
                    Nothing
                    (toInlStr <$> mnum)
                    mnote
      , concl
      )

genListTitle
  :: TitlingConfig (Inline Void)
  -> List Block (Inline Void)
  -> List Block (Inline Void)
genListTitle m l = case l of
  Ulist l' -> Ulist $ go l'
  Olist l' -> Olist $ go l'
  where go = map $ genMixedBlockBodyTitle m

genMixedBlockBodyTitle
  :: TitlingConfig (Inline Void)
  -> MixedBody Block (Inline Void)
  -> MixedBody Block (Inline Void)
genMixedBlockBodyTitle m (MixedBlock b) = MixedBlock $ map (genBlockTitle m) b
genMixedBlockBodyTitle _ x              = x

-- TODO: reduce duplication with genFormalTitle
genSectionTitle
  :: TitlingConfig (Inline Void)
  -> Section Block (Inline Void)
  -> Section Block (Inline Void)
genSectionTitle m (Section mty mtbody mtfull mnum c) =
  let c' = genSecContentTitle m c
  in  Section mty mtbody (mtfull <|> mtigen <|> mtbody) mnum c'
 where
  mtigen = do
    t     <- mty
    sconf <- M.lookup t $ tcSectionConfig m
    let template = sconfTitleTemplate sconf
        toInlStr = (: []) . Istr . Str
    Title <$> runTemplate template
                          ItitleComponent
                          (Istr $ Str " ")
                          FormalTemplate
                          Nothing
                          (toInlStr <$> mnum)
                          (titleBody <$> mtbody)
