{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.DisplayMath where

import           Text.Scriba.Intermediate
import           Text.Scriba.Decorate.Common
import           Text.Scriba.Decorate.Numbering
import           Text.Scriba.Decorate.Referencing
import           Text.Scriba.Decorate.Titling
import           Text.Scriba.Element.Identifier

import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

data MathItem = MathItem
  { miIdentifier :: Maybe Identifier
  , miNum :: Maybe Text
  , miIsNumbered :: Bool
  , miContent :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

instance Referencing i MathItem MathItem
instance Titling i MathItem

-- TODO: may need more complex numbering behaviour here.
instance Numbering i MathItem where
  numbering x@(MathItem _ Nothing False _) = pure x
  numbering (MathItem mId mnum t cont) = bracketNumbering (Just "formula") mId
    $ \mnumgen -> pure $ MathItem mId (mnum <|> mnumgen) t cont

mathItemToText :: MathItem -> [Text]
mathItemToText (MathItem _ _ _ t) = [t]

-- Parse only the content of a math item
pMathItem :: Scriba Element MathItem
pMathItem = do
  t                     <- T.concat <$> allContentOf simpleText
  (mId, mnumber, isNum) <- meta $ attrs $ do
    mId     <- attrMaybe "id" $ content pIdent
    mnumber <- attrMaybe "n" $ allContentOf simpleText
    mIsNum  <- attrMaybe "noNum" $ pure ()
    pure (mId, mnumber, not $ isJust mIsNum)
  pure $ MathItem mId (T.concat <$> mnumber) isNum t

-- Formulas have a possible identifier and a possible number
-- The Bool in gathered indicates if its content should be numbered or
-- not.

-- TODO: our "gathered" is more like "gather", I think, since it
-- appears in normal text and cannot be used within mathematical
-- content.
data DisplayMath
  = Formula MathItem
  | Gathered Bool [MathItem]
  deriving (Eq, Ord, Show, Read, Generic)

instance Referencing i DisplayMath DisplayMath
instance Titling i DisplayMath

-- TODO: have these be classes somewhere

displayMathToText :: DisplayMath -> [Text]
displayMathToText (Formula mi   ) = mathItemToText mi
displayMathToText (Gathered _ ts) = concatMap mathItemToText ts

-- TODO: syntactic unification with pMath? it's probably better to
-- have a single "display" parameter control both, and have dmath be a
-- syntactic alias (in some way) for math {presentation|display}
pFormula :: Scriba Element DisplayMath
pFormula = Formula <$> whileMatchTy "dmath" pMathItem

-- TODO: syntact unification with formula? May want to consider design
-- here. E.g. could have a single dmath whose content is flexibly
-- parsed, have Gathered be a list of math and not Text, that sort of
-- thing.
-- TODO: enforce non-emptiness of gathered?
pGathered :: Scriba Element DisplayMath
pGathered = whileMatchTy "gathered" $ do
  body   <- allContent $ pOnlySpace *> many pLines
  mIsNum <- meta $ attrs $ attrMaybe "noNum" $ pure ()
  pure $ Gathered (not $ isJust mIsNum) body
 where
  pLines = one (asNode pLine) <* pOnlySpace
  pLine  = whileMatchTy "line" $ pMathItem

instance Numbering i DisplayMath

