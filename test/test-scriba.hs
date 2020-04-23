{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Text.Scriba.Parse             as SP
import qualified Text.Scriba.Intermediate      as SI
import qualified Text.Scriba.Markup            as SM
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
import qualified Text.Show.Pretty              as SP
import qualified Text.Blaze.Html.Renderer.Text as HT

byteShow :: Show a => a -> BL.ByteString
byteShow = TLE.encodeUtf8 . TL.pack . SP.ppShow

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
 where
  go t = either (error . T.unpack) byteShow
    $ SP.parseDoc' (T.pack $ takeFileName src) t

testMarkup :: String -> FilePath -> FilePath -> TestTree
testMarkup name src gold = goldenWith go name src gold
 where
  go t =
    let n =
            either (error . T.unpack) id
              $   SI.fromDoc
              <$> SP.parseDoc' (T.pack $ takeFileName src) t
    in  either (error . show) byteShow $ SM.parseDoc n

-- TODO: duplication
testRenderingWith
  :: (SM.Doc -> TL.Text) -> String -> FilePath -> FilePath -> TestTree
testRenderingWith f name src gold = goldenWith go name src gold
 where
  go t =
    let n =
            either (error . T.unpack) id
              $   SI.fromDoc
              <$> SP.parseDoc' (T.pack $ takeFileName src) t
    in  either (error . show) (TLE.encodeUtf8 . f) $ SM.parseDoc n


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
  , testParse "manual parses" "./doc/manual.sml" "./test/tests/manual.parse"
  , testMarkup "manual parses correctly"
               "./doc/manual.sml"
               "./test/tests/manual.markup"
  , testRenderingWith (HT.renderHtml . SRH.renderStandalone)
                      "manual renders to html"
                      "./doc/manual.sml"
                      "./test/tests/manual.html"
  ]

main :: IO ()
main = defaultMain tests
