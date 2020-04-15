{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import qualified Text.Scriba.Parse             as SP

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


testDoc :: FilePath -> Expectation
testDoc fp = do
  let sml = "./test/tests/" <> fp <> ".sml"
      int = "./test/tests/" <> fp <> ".internal"
  tSml      <- T.readFile sml
  tInternal <- readFile int
  let ea   = SP.parseDoc' (T.pack $ fp <> ".sml") tSml
      -- TODO: safe read, I suppose?
      good = read tInternal
  case ea of
        -- TODO: something better here?
    Left  e -> error $ T.unpack e
    Right a -> a `shouldBe` good

main :: IO ()
main = hspec $ do
  describe "scriba" $ do
    it "parses the README example" $ testDoc "bayes"
    it "parses a verbatim block" $ testDoc "block-verbatim"



