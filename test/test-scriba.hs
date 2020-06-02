{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Scriba.Intermediate      as SI
import qualified Text.Scriba.Markup            as SM
import qualified Text.Scriba.Source.Common     as SC
import qualified Text.Scriba.Source.Parse      as SP

import           Data.ByteString.Lazy          as BL
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           Data.Void                      ( Void )
import           System.FilePath                ( takeFileName )
import           Test.Tasty
import           Test.Tasty.Golden
import qualified Text.Show.Pretty              as Pretty
import qualified Text.Blaze.Html.Renderer.Text as HT

parseOrExplode :: Text -> Text -> SC.Doc
parseOrExplode name t = case SP.parseDoc' name t of
  Left  e -> error $ T.unpack e
  Right a -> a

-- with decoration
-- TODO: nicer looking messages
markupOrExplode
  :: SI.Node
  -> ( Map SM.Identifier (SM.NoteText (SM.Block SM.Void1) (SM.Inline Void))
     , SM.Doc (SM.Block SM.Void1) (SM.Inline Void) (SM.Inline Void)
     )
markupOrExplode n = case SM.parseArticle n of
  Left  e -> error $ T.unpack $ SM.prettyScribaError e
  Right a -> case SM.decorateDoc a of
    Left  e  -> error $ show e
    Right a' -> a'

byteShow :: Show a => a -> BL.ByteString
byteShow = TLE.encodeUtf8 . TL.pack . Pretty.ppShow

-- TODO: findByExtension?
-- TODO: find a way to suppress the SourcePos output of these
-- tests. Should make the diffs a little more sane.
goldenWith
  :: (Text -> BL.ByteString) -> String -> FilePath -> FilePath -> TestTree
goldenWith f name src gold = goldenVsString name gold $ do
  t <- T.readFile src
  pure $ f t

inTests :: (b -> FilePath -> FilePath -> a) -> b -> FilePath -> FilePath -> a
inTests f x p q = f x ("./test/tests/" <> p) ("./test/tests/" <> q)

testParse :: String -> FilePath -> FilePath -> TestTree
testParse name src gold = goldenWith go name src gold
  where go t = byteShow $ parseOrExplode (T.pack $ takeFileName src) t

testIntermediate :: String -> FilePath -> FilePath -> TestTree
testIntermediate name src gold = goldenWith go name src gold
 where
  go t = byteShow $ SI.fromDoc $ parseOrExplode (T.pack $ takeFileName src) t

testRenderingWith
  :: (  ( Map SM.Identifier (SM.NoteText (SM.Block SM.Void1) (SM.Inline Void))
       , SM.Doc (SM.Block SM.Void1) (SM.Inline Void) (SM.Inline Void)
       )
     -> TL.Text
     )
  -> String
  -> FilePath
  -> FilePath
  -> TestTree
testRenderingWith f name src gold = goldenWith go name src gold
 where
  go t = TLE.encodeUtf8 $ f $ markupOrExplode $ SI.fromDoc $ parseOrExplode
    (T.pack $ takeFileName src)
    t

-- TODO: make sure the README example parses _correctly_.
tests :: TestTree
tests = testGroup
  "tests"
  [ inTests testParse "README example parses" "bayes.scb"  "bayes.parse"
  , inTests testParse "simple markup parses"  "simple.scb" "simple.parse"
  , inTests testIntermediate
            "simple markup parses into the node format"
            "simple.scb"
            "simple.intermediate"
  , testRenderingWith
    (HT.renderHtml . SM.writeStandalone (SM.StandaloneConfig "./manual.css"))
    "manual renders to html"
    "./doc/manual/manual.scb"
    "./doc/manual/manual.html"
  , testRenderingWith
    (HT.renderHtml . SM.writeStandalone (SM.StandaloneConfig "./riemann.css"))
    "riemann renders to html"
    "./doc/riemann/riemann.scb"
    "./doc/riemann/riemann.html"
  ]

main :: IO ()
main = defaultMain tests
