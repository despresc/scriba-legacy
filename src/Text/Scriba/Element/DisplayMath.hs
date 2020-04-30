{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Element.DisplayMath where

import           Text.Scriba.Intermediate
import           Text.Scriba.Numbering

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

data DisplayMath
  = Formula Text
  | Gathered [Text]
  deriving (Eq, Ord, Show, Read, Generic)

instance Numbering DisplayMath

-- TODO: have these be classes somewhere

displayMathToText :: DisplayMath -> [Text]
displayMathToText (Formula  t ) = [t]
displayMathToText (Gathered ts) = ts

-- TODO: syntactic unification with pMath? it's probably better to
-- have a single "display" parameter control both, and have dmath be a
-- syntactic alias (in some way) for math {presentation|display}
pFormula :: Scriba Element DisplayMath
pFormula =
  Formula . T.concat <$> whileMatchTy "dmath" (allContentOf simpleText)

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
