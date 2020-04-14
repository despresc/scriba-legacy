{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import qualified Text.Scriba.Parse             as SP

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

main :: IO ()
main = hspec $ do
  describe "scriba" $ do
    it "parses the README example" $ do
      t     <- T.readFile "./test/tests/bayes.sml"
      tGood <- readFile "./test/tests/bayes.internal"
      let ea    = SP.parseDoc' "bayes.sml" t
          -- TODO: safe read, I suppose?
          aGood = read tGood
      case ea of
        -- TODO: something better here?
        Left  e -> error $ T.unpack e
        Right a -> a `shouldBe` aGood


