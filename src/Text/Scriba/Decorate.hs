{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Decorate where

-- Decorate the internal representation in various ways.

import           Text.Scriba.Markup

import           Control.Applicative            ( (<|>) )
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )

-- TODO: may need errors, a state for numbering, environment for
-- titling. Or perhaps pipelined for modularity, with a shared error
-- type?
decorate :: Doc -> Doc
decorate (Doc met f m b) = Doc met (go f) (go m) (go b)
  where go = genSecContentTitle $ docFormalConfig met

-- TODO: really shows that a Walkable class is necessary.
genSecContentTitle :: Map Text FormalConfig -> SectionContent -> SectionContent
genSecContentTitle m (SectionContent p c) =
  SectionContent (genBlockTitle m <$> p) (genSectionTitle m <$> c)

-- TODO: we don't walk any inlines because there is nothing to
-- generate for them. That might change!
genBlockTitle :: Map Text FormalConfig -> Block -> Block
genBlockTitle m (FormalBlock formal) = FormalBlock $ genFormalTitle m formal
genBlockTitle m (ListBlock   l     ) = ListBlock $ genListTitle m l
genBlockTitle _ x                    = x

genFormalTitle :: Map Text FormalConfig -> Formal -> Formal
genFormalTitle m (Formal mty mnum mti mnote cont conc) = Formal
  mty
  mnum
  (mti <|> mtigen)
  mnote
  cont
  conc
 where
  mtigen = do
    t     <- mty
    fconf <- M.lookup t m
    let
      pref     = fconfPrefix fconf
      template = fconfTitleTemplate fconf
      num      = maybe [] ((: []) . Str) mnum
      note     = fromMaybe [] mnote
      vars =
        M.fromList $ [("titlePrefix", pref), ("titleNote", note), ("n", num)]
    pure $ runVariedInline vars template

genListTitle :: Map Text FormalConfig -> List -> List
genListTitle m l = case l of
  Ulist l' -> Ulist $ go l'
  Olist l' -> Olist $ go l'
  where go = map $ map $ genBlockTitle m

-- TODO: should probably generate section titles too!
genSectionTitle :: Map Text FormalConfig -> Section -> Section
genSectionTitle m (Section t c) = Section t $ genSecContentTitle m c
