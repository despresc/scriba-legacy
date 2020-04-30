{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.OldDecorate where

-- Decorate the internal representation in various ways.

import           Text.Scriba.Markup
import           Text.Scriba.Numbering

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( join )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import           Data.Void

-- TODO: may need errors, a state for numbering, environment for
-- titling. Or perhaps pipelined for modularity, with a shared error
-- type?
decorate :: Doc Block (Inline Void) -> Doc Block (Inline Void)
decorate = genDocTitle . numDoc

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

-- TODO: Doesn't number anything in the config. Should it?
numDoc :: Doc Block (Inline i) -> Doc Block (Inline i)
numDoc (Doc da f m b) = flip runNumbering (defaultNumberState da) $ do
  f' <- numSectionContent f
  m' <- numSectionContent m
  b' <- numSectionContent b
  pure $ Doc da f' m' b'

numSectionContent :: SectionContent Block (Inline i) -> Numbering (SectionContent Block (Inline i))
numSectionContent (SectionContent p c) = do
  p' <- numBlocks p
  c' <- numSections c
  pure $ SectionContent p' c'

numBlocks :: [Block (Inline a)] -> Numbering [Block (Inline a)]
numBlocks = traverse numBlock

numSections :: [Section Block (Inline i)] -> Numbering [Section Block (Inline i)]
numSections = traverse numSection

-- TODO: integrate list numbering into all of this.
numBlock :: Block (Inline a) -> Numbering (Block (Inline a))
numBlock (Bformal formal) = Bformal <$> numFormal formal
numBlock (Blist   l     ) = Blist <$> numList l
numBlock x                = pure x

-- TODO: duplication with numFormal
numSection :: Section Block (Inline i) -> Numbering (Section Block (Inline i))
numSection (Section mty tbody tfull mnum c) = bracketNumbering mty $ \mnumgen -> do
  tbody'  <- traverse numTitle tbody
  tfull'  <- traverse numTitle tfull
  c'      <- numSectionContent c
  pure $ Section mty tbody' tfull' (mnum <|> mnumgen) c'

-- TODO: we don't skip numbering a formal block when it already has a
-- number. Should have config for that sort of thing.
numFormal :: Formal Block (Inline a) -> Numbering (Formal Block (Inline a))
numFormal (Formal mty mnum ti note tsep cont concl) = bracketNumbering mty $ \mnumgen -> do
  ti'     <- traverse (numInlinesWith pure) ti
  note'   <- traverse numInlines note
  tsep'   <- traverse numInlines tsep
  cont'   <- numMixedBlockBody cont
  concl'  <- traverse numInlines concl
  pure $ Formal mty (mnum <|> mnumgen) ti' note' tsep' cont' concl'

numList :: List Block (Inline a) -> Numbering (List Block (Inline a))
numList (Ulist l) = Ulist <$> traverse numMixedBlockBody l
numList (Olist l) = Olist <$> traverse numMixedBlockBody l

-- TODO: we don't descend into Iother, even though it might be
-- possible to have numbered inline things.
numTitle :: Title a -> Numbering (Title a)
numTitle = pure

numInlinesWith :: (a -> Numbering a) -> [Inline a] -> Numbering [Inline a]
numInlinesWith _ = pure

numInlines :: [Inline a] -> Numbering [Inline a]
numInlines = numInlinesWith pure

numMixedBlockBody
  :: MixedBody Block (Inline a) -> Numbering (MixedBody Block (Inline a))
numMixedBlockBody (MixedInline p) = MixedInline <$> numInlines p
numMixedBlockBody (MixedBlock  b) = MixedBlock <$> numBlocks b

numInline :: Inline a -> Numbering (Inline a)
numInline = pure


-- * Generating titles

genDocTitle :: Doc Block (Inline Void) -> Doc Block (Inline Void)
genDocTitle (Doc met f m b) = Doc met (go f) (go m) (go b)
 where
  dfc = docTitlingConfig met
  go  = genSecContentTitle dfc

-- TODO: Make a Reader for this stuff

-- TODO: really shows that a Walkable class is necessary.
genSecContentTitle :: TitlingConfig (Inline Void) -> SectionContent Block (Inline Void) -> SectionContent Block (Inline Void)
genSecContentTitle m (SectionContent p c) =
  SectionContent (genBlockTitle m <$> p) (genSectionTitle m <$> c)

-- TODO: we don't walk any inlines because there is nothing to
-- generate for them. That might change!
genBlockTitle :: TitlingConfig (Inline Void) -> Block (Inline Void) -> Block (Inline Void)
genBlockTitle m (Bformal formal) = Bformal $ genFormalTitle m formal
genBlockTitle m (Blist   l     ) = Blist $ genListTitle m l
genBlockTitle _ x                = x

-- TODO: this also generates the conclusion of formal blocks. Sort of
-- misleading that it happens here, perhaps...
genFormalTitle
  :: TitlingConfig (Inline Void) -> Formal Block (Inline Void) -> Formal Block (Inline Void)
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
      , runTemplate template ItitleComponent (Istr $ Str " ") FormalTemplate Nothing (toInlStr <$> mnum) mnote
      , concl
      )

genListTitle
  :: TitlingConfig (Inline Void) -> List Block (Inline Void) -> List Block (Inline Void)
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
genSectionTitle :: TitlingConfig (Inline Void) -> Section Block (Inline Void) -> Section Block (Inline Void)
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
