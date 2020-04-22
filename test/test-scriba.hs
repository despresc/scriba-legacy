{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import qualified Text.Scriba.Parse             as SP
import qualified Text.Scriba.Intermediate      as SI
import qualified Text.Scriba.Markup            as SM

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import System.FilePath (takeFileName)


parsesAs :: FilePath -> FilePath -> Expectation
parsesAs sml int = do
  tSml      <- T.readFile sml
  tInternal <- readFile int
  let ea   = SP.parseDoc' (T.pack $ takeFileName sml) tSml
      -- TODO: safe read, I suppose?
      good = read tInternal
  case ea of
    -- TODO: something better here?
    Left  e -> error $ T.unpack e
    Right a -> a `shouldBe` good

testParse :: FilePath -> Expectation
testParse fp = do
  let sml = "./test/tests/" <> fp <> ".sml"
      int = "./test/tests/" <> fp <> ".parse"
  sml `parsesAs` int

-- TODO: duplication
testMarkup :: FilePath -> Expectation
testMarkup fp = do
  let sml = "./test/tests/" <> fp <> ".sml"
      int = "./test/tests/" <> fp <> ".markup"
  tSml      <- T.readFile sml
  tInternal <- readFile int
  let ea = do
        a <- SP.parseDoc' (T.pack $ fp <> ".sml") tSml
        let a' = SI.fromDoc a
            toText = T.pack . show
        either (Left . toText) Right $ SM.parseDoc a'
      -- TODO: safe read, I suppose?
      good = read tInternal
  case ea of
    -- TODO: something better here?
    Left  e -> error $ T.unpack e
    Right a -> a `shouldBe` good

main :: IO ()
main = hspec $ do
  describe "scriba" $ do
    it "parses the README example" $ testParse "bayes"
    it "parses a verbatim block" $ testParse "block-verbatim"
    it "parses a simple example correctly" $ testMarkup "simple"
    it "parses the manual" $ "./doc/manual.sml" `parsesAs` "./test/tests/manual.parse"
