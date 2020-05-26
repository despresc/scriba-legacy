{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Note where

import           Text.Scriba.Decorate
import           Text.Scriba.Element.Identifier
import           Text.Scriba.Element.MixedBody
import           Text.Scriba.Intermediate

import           Control.Monad.Except           ( MonadError(..) )
import           GHC.Generics                   ( Generic )

-- TODO: HERE
-- Add:
-- 2. the types and parsers to Inline
-- 3. note mark numbering
-- 4. note text gathering in Linking? That might require polymorphic LinkData though
-- 5. footnote placement in a rendered MemDoc

data NoteMark = NoteMark
  { noteTarget :: Identifier
  } deriving ( Eq
             , Ord
             , Show
             , Read
             , Generic
             , Numbering
             , Titling i
             , Referencing NoteMark )

instance Gathering note NoteMark NoteMark where
  gathering n@(NoteMark (Identifier i)) = do
    tellLinkGen "" (Just $ Identifier $ "noteMark-" <> i)
    pure n

data NoteText b i = NoteText
  { noteSource :: Identifier
  , noteNum :: Maybe Int
  , noteText :: MixedBody b i
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

instance (Titling a (b i), Titling a i) => Titling a (NoteText b i)
instance ( Referencing (f a) (g b)
         , Referencing a b
         ) => Referencing (NoteText f a) (NoteText g b)
instance (Numbering (b i), Numbering i) => Numbering (NoteText b i) where
  numbering (NoteText i _ c) = bracketNumbering' (Just "noteText") $ \mna -> do
    c' <- numbering c
    pure $ NoteText i (getNum <$> mna) c'
    where getNum (NumberAuto _ _ n _) = n

resolveBlockNoteText :: (HasNil (b' i'), Gathering (NoteText b' i') (b i) (b' i'), Gathering (NoteText b' i') i i') => NoteText b i -> GatherM (NoteText b' i') (b' i')
resolveBlockNoteText (NoteText (Identifier i) mn c) = do
  tellLinkGen "" (Just $ Identifier $ "noteText-" <> i)
  c' <- gathering c
  tellNoteText (Identifier i) $ NoteText (Identifier i) mn c'
  pure embedNil

-- duplication with Ref.pSourceRef and pNoteText
pNoteMark :: Scriba Element NoteMark
pNoteMark = whileMatchTy "noteMark" $ do
  as <- meta $ args inspect
  case as of
    [t] -> useState [t] $ NoteMark <$> pIdent
    _   -> throwError $ Msg "ref takes exactly one identifier as an argument"

pNoteText :: Scriba Node (b i) -> Scriba Node i -> Scriba Element (NoteText b i)
pNoteText pBlk pInl = whileMatchTy "noteText" $ do
  as <- meta $ args inspect
  i  <- case as of
    [t] -> useState [t] pIdent
    _   -> throwError $ Msg "ref takes exactly one identifier as an argument"
  c <- allContent $ pMixedBody pBlk pInl
  pure $ NoteText i Nothing c