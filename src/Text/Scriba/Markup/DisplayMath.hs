module Text.Scriba.Markup.DisplayMath where

import           Data.Text                      ( Text )

data DisplayMath
  = Formula Text
  | Gathered [Text]
  deriving (Eq, Ord, Show, Read)

-- TODO: have these be classes somewhere

displayMathToText :: DisplayMath -> [Text]
displayMathToText (Formula  t ) = [t]
displayMathToText (Gathered ts) = ts
