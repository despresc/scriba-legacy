{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Text.Scriba.Decorate          as SD
import qualified Text.Scriba.Intermediate      as SI
import qualified Text.Scriba.Markup            as SM
import qualified Text.Scriba.Parse             as SP
import qualified Text.Scriba.Render.Html       as SRH

import           Data.ByteString.Lazy          as BL
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           System.FilePath                ( takeFileName )
import           Test.Tasty
import           Test.Tasty.Golden
import qualified Text.Show.Pretty              as Pretty
import qualified Text.Blaze.Html.Renderer.Text as HT

parseOrExplode :: Text -> Text -> SP.Doc
parseOrExplode name t = case SP.parseDoc' name t of
  Left  e -> error $ T.unpack e
  Right a -> a

-- with decoration
markupOrExplode :: SI.Node -> SM.Doc
markupOrExplode n = case SM.parseDoc n of
  Left  e -> error $ T.unpack $ SM.prettyScribaError e
  Right a -> SD.decorate a

byteShow :: Show a => a -> BL.ByteString
byteShow = TLE.encodeUtf8 . TL.pack . Pretty.ppShow

-- TODO: findByExtension?
goldenWith
  :: (Text -> BL.ByteString) -> String -> FilePath -> FilePath -> TestTree
goldenWith f name src gold = goldenVsString name gold $ do
  t <- T.readFile src
  pure $ f t

inTests :: (b -> FilePath -> FilePath -> a) -> b -> FilePath -> FilePath -> a
inTests f x p q = f x ("./test/tests/" <> p) ("./test/tests/" <> q)

-- TODO: should probably have a few pipeline functions in the top
-- level. Would reduce duplication.
testParse :: String -> FilePath -> FilePath -> TestTree
testParse name src gold = goldenWith go name src gold
  where go t = byteShow $ parseOrExplode (T.pack $ takeFileName src) t

testIntermediate :: String -> FilePath -> FilePath -> TestTree
testIntermediate name src gold = goldenWith go name src gold
 where
  go t = byteShow $ SI.fromDoc $ parseOrExplode (T.pack $ takeFileName src) t

testMarkup :: String -> FilePath -> FilePath -> TestTree
testMarkup name src gold = goldenWith go name src gold
 where
  go t = byteShow $ markupOrExplode $ SI.fromDoc $ parseOrExplode
    (T.pack $ takeFileName src)
    t

-- TODO: duplication
testRenderingWith
  :: (SM.Doc -> TL.Text) -> String -> FilePath -> FilePath -> TestTree
testRenderingWith f name src gold = goldenWith go name src gold
 where
  go t = TLE.encodeUtf8 $ f $ markupOrExplode $ SI.fromDoc $ parseOrExplode
    (T.pack $ takeFileName src)
    t

-- TODO: one single test block for the manual?
tests :: TestTree
tests = testGroup
  "tests"
  [ inTests testParse "README example parses" "bayes.sml" "bayes.parse"
  , inTests testParse
            "verbatim block parses"
            "block-verbatim.sml"
            "block-verbatim.parse"
  , inTests testMarkup
            "simple markup parses correctly"
            "simple.sml"
            "simple.markup"
  , testParse "manual parses" "./doc/manual.scb" "./test/tests/manual.parse"
  , testIntermediate "manual parses into the node format"
                     "./doc/manual.scb"
                     "./test/tests/manual.intermediate"
  , testMarkup "manual parses into internal markup"
               "./doc/manual.scb"
               "./test/tests/manual.markup"
  , testRenderingWith (HT.renderHtml . SRH.writeStandalone)
                      "manual renders to html"
                      "./doc/manual.scb"
                      "./test/tests/manual.html"
  ]

main :: IO ()
main = defaultMain tests
