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
import qualified Text.Scriba.Render.Html       as RH

import           Control.Monad.Except           ( MonadError(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

newtype SourceNoteMark = SourceNoteMark
  { sourceNoteTarget :: Identifier
  } deriving ( Eq
             , Ord
             , Show
             , Read
             , Generic
             , Numbering
             , Titling i
             , Referencing SourceNoteMark )

data NoteMark = NoteMark
  { noteMarkTarget :: Identifier
  , noteMarkNum :: Int -- Ignoring style and other information right now.
  } deriving (Eq, Ord, Show, Read, Generic)

-- Dummy instances because note marks should never occur in the tree
-- naturally
instance Numbering NoteMark
instance Referencing NoteMark NoteMark
instance Titling i NoteMark
instance Gathering note NoteMark NoteMark

instance Gathering note SourceNoteMark SourceNoteMark where
  gathering n@(SourceNoteMark (Identifier i)) = do
    let i' = Identifier $ "noteMark-" <> i
    tellLinkGen "" (Just i')
    pure n

-- TODO: instead just have the n inside? probably for the best,
-- esp. with changing footnote reference styles. Otherwise there
-- should be configuration.
instance RH.Render NoteMark where
  render (NoteMark i n) = pure $ do
    let i'    = HtmlA.href $ identAttrVal $ prefixIdent "#noteText-" i
        ident = identAttr $ prefixIdent "noteMark-" i
    Html.a Html.! HtmlA.class_ "noteMark" Html.! ident Html.! i' $ Html.toHtml n

data NoteText b i = NoteText
  { noteIdentifier :: Identifier
  , noteNum :: Maybe NumberAuto
  , noteText :: MixedBody b i
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

instance (Titling a (b i), Titling a i) => Titling a (NoteText b i)
instance ( Referencing (f a) (g b)
         , Referencing a b
         ) => Referencing (NoteText f a) (NoteText g b)
instance (Numbering (b i), Numbering i) => Numbering (NoteText b i) where
  numbering (NoteText i _ c) = bracketNumbering' (Just "noteText") $ \mna -> do
    c' <- numbering c
    pure $ NoteText i mna c'

-- TODO: warn here about a Nothing number?
gatheringBlockNoteText
  :: ( HasNil (b' i')
     , Gathering (NoteText b' i') (b i) (b' i')
     , Gathering (NoteText b' i') i i'
     )
  => NoteText b i
  -> GatherM (NoteText b' i') (b' i')
gatheringBlockNoteText (NoteText i mn c) = do
  let itext = prefixIdent "noteText-" i
  tellLinkNumbered "" (Just itext) (ElemNumberAuto <$> mn)
  c' <- gathering c
  tellNoteText i $ NoteText i mn c'
  pure embedNil

resolveNoteMark
  :: (MonadError DecorateError m, MonadLibResolve m)
  => SourceNoteMark
  -> m NoteMark
resolveNoteMark = undefined
{- TODO library : restore
resolveNoteMark (SourceNoteMark i) = do
  ld <- lookupRefData $ RefSelf $ prefixIdent "noteText-" i
  case ld of
    LinkNumber _ _ (ElemNumberAuto (NumberAuto _ _ n _)) -> pure $ NoteMark i n
    _ -> throwError
      $ DecorateError "internal logic error - note not automatically numbered"
-}

-- duplication with Ref.pSourceRef and pNoteText
pSourceNoteMark :: Scriba Element SourceNoteMark
pSourceNoteMark = whileMatchTy "noteMark" $ do
  as <- meta $ args inspect
  case as of
    [t] -> useState [t] $ SourceNoteMark <$> pIdent
    _ ->
      throwError $ Msg "noteMark takes exactly one identifier as an argument"

pNoteText :: Scriba Node (b i) -> Scriba Node i -> Scriba Element (NoteText b i)
pNoteText pBlk pInl = whileMatchTy "noteText" $ do
  as <- meta $ args inspect
  i  <- case as of
    [t] -> useState [t] pIdent
    _ ->
      throwError $ Msg "noteText takes exactly one identifier as an argument"
  c <- allContent $ pMixedBody pBlk pInl
  pure $ NoteText i Nothing c

-- TODO: not sure about this
noteMarkToText :: NoteMark -> [Text]
noteMarkToText (NoteMark (Identifier i) n) =
  ["[" <> T.pack (show n) <> "; #" <> i <> "]"]
