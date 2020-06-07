{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.Table where

import           Text.Scriba.Decorate
import           Text.Scriba.Intermediate
import qualified Text.Scriba.Render.Html       as RH

import           Control.Applicative            ( optional )
import           Control.Monad                  ( zipWithM )
import           Control.Monad.Except           ( throwError )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as HtmlA

data SimpleTable (b :: * -> *) i = SimpleTable
  { simpleTableColSpec :: ColSpec
  , simpleTableHead :: [HeadRow i]
  , simpleTableBody :: [BodyRow i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor, Numbering, Titling a)

instance Referencing i i' => Referencing (SimpleTable b i) (SimpleTable b' i')
instance Gathering note i i' => Gathering note (SimpleTable b i) (SimpleTable b' i')
instance RH.Render i => RH.Render (SimpleTable b i) where
  render (SimpleTable cs h b) = do
    h' <- traverse (renderHeadRow cs) h
    b' <- traverse (renderBodyRow cs) b
    pure
      $      Html.div
      Html.! HtmlA.class_ "simpleTable"
      $      Html.table
      $      Html.thead (mconcat h')
      <>     Html.tbody (mconcat b')

newtype HeadRow i = HeadRow [HeadCell i]
  deriving (Eq, Ord, Show, Read, Generic, Functor)
  deriving anyclass (Numbering, Titling a)

instance Referencing i i' => Referencing (HeadRow i) (HeadRow i')
instance Gathering note i i' => Gathering note (HeadRow i) (HeadRow i')

renderHeadRow :: RH.Render i => ColSpec -> HeadRow i -> RH.RenderM Html.Html
renderHeadRow (ColSpec cs) (HeadRow hs) =
  Html.tr . mconcat <$> zipWithM renderHeadCell cs hs

newtype HeadCell i = HeadCell
  { headCellBody :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)
    deriving anyclass (Numbering, Titling a)

instance Referencing i i' => Referencing (HeadCell i) (HeadCell i')
instance Gathering note i i' => Gathering note (HeadCell i) (HeadCell i')

renderHeadCell :: RH.Render i => ColData -> HeadCell i -> RH.RenderM Html.Html
renderHeadCell cs (HeadCell b) = do
  b' <- RH.render b
  pure $ Html.th Html.! HtmlA.class_ (Html.toValue $ colDataClass cs) $ b'

newtype BodyRow i = BodyRow [BodyCell i]
  deriving (Eq, Ord, Show, Read, Generic, Functor)
  deriving anyclass (Numbering, Titling a)

instance Referencing i i' => Referencing (BodyRow i) (BodyRow i')
instance Gathering note i i' => Gathering note (BodyRow i) (BodyRow i')

renderBodyRow :: RH.Render i => ColSpec -> BodyRow i -> RH.RenderM Html.Html
renderBodyRow (ColSpec cs) (BodyRow hs) =
  Html.tr . mconcat <$> zipWithM renderBodyCell cs hs

newtype BodyCell i = BodyCell
  { bodyCellBody :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)
    deriving anyclass (Numbering, Titling a)

instance Referencing i i' => Referencing (BodyCell i) (BodyCell i')
instance Gathering note i i' => Gathering note (BodyCell i) (BodyCell i')

renderBodyCell :: RH.Render i => ColData -> BodyCell i -> RH.RenderM Html.Html
renderBodyCell cs (BodyCell b) = do
  b' <- RH.render b
  pure $ Html.td Html.! HtmlA.class_ (Html.toValue $ colDataClass cs) $ b'

newtype ColSpec = ColSpec [ColData]
  deriving (Eq, Ord, Show, Read, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (Numbering, Titling a, Gathering note ColSpec, Referencing ColSpec)

newtype ColData = ColData Alignment
  deriving (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Numbering, Titling a, Gathering note ColData)

instance Referencing ColData ColData

colDataClass :: ColData -> Text
colDataClass (ColData d) = case d of
  AlignLeft    -> "alignLeft"
  AlignRight   -> "alignRight"
  AlignCenter  -> "alignCenter"
  AlignDefault -> "alignDefault"

data Alignment = AlignLeft | AlignRight | AlignCenter | AlignDefault
  deriving (Eq, Ord, Show, Read, Generic, Numbering, Titling a, Gathering note Alignment)

instance Referencing Alignment Alignment

pSimpleTable :: Scriba Node i -> Scriba Element (SimpleTable b i)
pSimpleTable pInl = whileMatchTy "simpleTable" $ do
  colspec <- meta $ attrs $ mattr "columns" pColSpec
  content $ do
    mhd <- optional $ pHead pInl
    let hd = fromMaybe [] mhd
    mbd <- optional $ pBody pInl
    let bd = fromMaybe [] mbd
    zero
    let tbl = SimpleTable colspec hd bd
    guardValidTable tbl
    pure tbl

-- TODO: actually validate
guardValidTable :: SimpleTable b i -> Scriba s ()
guardValidTable _ = pure ()

pColSpec :: Scriba Element ColSpec
pColSpec = meta $ args $ ColSpec <$> remaining pColData
 where
  pAlign = do
    t <- ty inspect
    case t of
      Just "left"    -> pure AlignLeft
      Just "right"   -> pure AlignRight
      Just "center"  -> pure AlignCenter
      Just "default" -> pure AlignDefault
      Just x ->
        throwError $ Msg $ "column alignment: " <> x <> " not recognized"
      Nothing -> throwError $ Msg "column alignment must be present"
  pColData = ColData <$> asNode pAlign

-- TODO: allow whitespace
pHead :: Scriba Node i -> Scriba [Node] [HeadRow i]
pHead pInl = one $ asNode $ whileMatchTy "head" $ allContentOf $ asNode
  pHeadRow
 where
  pHeadRow  = whileMatchTy "row" $ HeadRow <$> allContentOf (asNode pHeadCell)
  pHeadCell = whileMatchTy "cell" $ do
    c <- allContentOf pInl
    pure $ HeadCell c

pBody :: Scriba Node i -> Scriba [Node] [BodyRow i]
pBody pInl = one $ asNode $ whileMatchTy "body" $ allContentOf $ asNode
  pBodyRow
 where
  pBodyRow  = whileMatchTy "row" $ BodyRow <$> allContentOf (asNode pBodyCell)
  pBodyCell = whileMatchTy "cell" $ BodyCell <$> allContentOf pInl
