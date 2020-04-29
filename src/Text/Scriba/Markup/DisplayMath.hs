{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Markup.DisplayMath where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Text.Scriba.Intermediate

data DisplayMath
  = Formula Text
  | Gathered [Text]
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: have these be classes somewhere

displayMathToText :: DisplayMath -> [Text]
displayMathToText (Formula  t ) = [t]
displayMathToText (Gathered ts) = ts

-- TODO: syntactic unification with pMath? it's probably better to
-- have a single "display" parameter control both, and have dmath be a
-- syntactic alias (in some way) for math {presentation|display}
pFormula :: Scriba Element DisplayMath
pFormula = do
  matchTy "dmath"
  c <- whileParsingElem "dmath" $ allContentOf simpleText
  pure $ Formula $ T.concat c

-- TODO: syntact unification with formula? May want to consider design
-- here. E.g. could have a single dmath whose content is flexibly
-- parsed, have Gathered be a list of math and not Text, that sort of
-- thing.
pGathered :: Scriba Element DisplayMath
pGathered = do
  matchTy "gathered"
  c <- whileParsingElem "gathered" $ allContent $ pOnlySpace *> many
    (one pLine <* pOnlySpace)
  pure $ Gathered c
 where
  pLine = asNode $ do
    matchTy "line"
    fmap T.concat $ whileParsingElem "line" $ allContentOf simpleText
