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

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

-- Formulas have a possible identifier and a possible number
data DisplayMath
  = Formula (Maybe Identifier) (Maybe Text) Text
  | Gathered [Text]
  deriving (Eq, Ord, Show, Read, Generic)

instance Referencing i DisplayMath DisplayMath
instance Titling i DisplayMath

-- TODO: have these be classes somewhere

displayMathToText :: DisplayMath -> [Text]
displayMathToText (Formula _ _ t) = [t]
displayMathToText (Gathered ts  ) = ts

-- TODO: syntactic unification with pMath? it's probably better to
-- have a single "display" parameter control both, and have dmath be a
-- syntactic alias (in some way) for math {presentation|display}
pFormula :: Scriba Element DisplayMath
pFormula = whileMatchTy "dmath" $ do
  t              <- T.concat <$> allContentOf simpleText
  (mId, mnumber) <- meta $ attrs $ do
    mId     <- attrMaybe "id" $ content pIdent
    mnumber <- attrMaybe "n" $ allContentOf simpleText
    pure (mId, mnumber)
  pure $ Formula mId (T.concat <$> mnumber) t

-- TODO: syntact unification with formula? May want to consider design
-- here. E.g. could have a single dmath whose content is flexibly
-- parsed, have Gathered be a list of math and not Text, that sort of
-- thing.
pGathered :: Scriba Element DisplayMath
pGathered = Gathered
  <$> whileMatchTy "gathered" (allContent $ pOnlySpace *> many pLines)
 where
  pLines = one pLine <* pOnlySpace
  pLine =
    fmap T.concat $ asNode $ whileMatchTy "line" $ allContentOf simpleText

instance Numbering i DisplayMath where
  numbering (Formula mId mnum cont) = bracketNumbering (Just "dmath") mId
    $ \mnumgen -> pure $ Formula mId (mnum <|> mnumgen) cont
  numbering x@Gathered{} = pure x
