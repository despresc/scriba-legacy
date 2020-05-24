{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Scriba.Element.List where

import           Text.Scriba.Decorate
import           Text.Scriba.Element.Identifier
import           Text.Scriba.Element.MixedBody
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

-- TODO: need to be able to configure unordered list items too.
data List b i
  = Ulist [MixedBody b i]
  | Olist [OlistItem b i]
  deriving (Eq, Ord, Show, Read, Generic, Functor, Titling a, Linking)

data OlistItem b i = OlistItem
  { olLabel :: Maybe Identifier
  , olNum :: Maybe ElemNumber
  , olContent :: MixedBody b i
  } deriving (Eq, Ord, Show, Read, Generic, Functor, Titling a, Numbering)

instance (Linking i, Linking (b i)) => Linking (OlistItem b i) where
  linking (OlistItem mi mnum c) = do
    tellLinkNumbered "" mi mnum
    linking c

-- TODO: not the best. We could have a "type inference" pass that
-- annotates the list items with the type of their parent
-- TODO: I think only a resetCounter is necessary here.
instance (Numbering (b i), Numbering i) => Numbering (List b i) where
  numbering (Olist items) = do
    resetCounter "item:olist"
    Olist <$> traverse numberItem items
   where
    numberItem (OlistItem mId mnum cont) =
      bracketNumbering (Just "item:olist") $ \mnumgen -> do
        cont' <- numbering cont
        pure $ OlistItem mId (mnum <|> mnumgen) cont'
  numbering (Ulist items) = Ulist <$> traverse numbering items

instance ( Referencing (f a) (g b)
         , Referencing a b
         ) => Referencing (List f a) (List g b)

instance ( Referencing (f a) (g b)
         , Referencing a b
         ) => Referencing (OlistItem f a) (OlistItem g b)

pList :: Scriba [Node] (MixedBody b i) -> Scriba Element (List b i)
pList p = pOlist p <|> pUlist p

pOlist :: Scriba [Node] (MixedBody b i) -> Scriba Element (List b i)
pOlist p = do
  matchTy "olist"
  content consumeWhiteSpace
  fmap Olist $ whileParsingElem "olist" $ allContent $ many
    (one pOlistItem <* consumeWhiteSpace)
 where
  pOlistItem = asNode $ whileParsingElem "item" $ do
    (mId, mnumber) <- meta $ attrs $ do
      mId     <- attrMaybe "id" $ content pIdent
      mnumber <- attrMaybe "n" $ allContentOf simpleText
      pure (mId, mnumber)
    cont <- content p
    pure $ OlistItem mId (NumberSource . T.concat <$> mnumber) cont

pUlist :: Scriba [Node] (MixedBody b i) -> Scriba Element (List b i)
pUlist p = do
  matchTy "ulist"
  content consumeWhiteSpace
  fmap Ulist $ whileParsingElem "ulist" $ allContent $ many
    (one (pListItem p) <* consumeWhiteSpace)

pListItem :: Scriba [Node] (MixedBody b i) -> Scriba Node (MixedBody b i)
pListItem p = asNode pItem
 where
  pItem = do
    matchTy "item"
    whileParsingElem "item" $ content p

instance (RH.Render (b i), RH.Render i) => RH.Render (List b i) where
  render b = case b of
    Ulist l -> Html.ul <$> renderListItems l
    Olist l -> Html.ol <$> RH.render l
   where
    renderListItems = RH.foldBy renderListItem
    renderListItem bs = Html.li <$> RH.render bs

instance (RH.Render (b i), RH.Render i) => RH.Render (OlistItem b i) where
  render (OlistItem mId _ c) = do
    c' <- RH.render c
    pure $ Html.li RH.?? ident $ c'
    where ident = (\(Identifier i) -> HtmlA.id (Html.toValue i)) <$> mId
