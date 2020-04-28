{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Text.Scriba.Counters          as SC
import qualified Text.Scriba.Decorate          as SD
import qualified Text.Scriba.Intermediate      as SI
import qualified Text.Scriba.Markup            as SM
import qualified Text.Scriba.Parse             as SP
import qualified Text.Scriba.Render.Html       as SRH

import           Data.ByteString.Lazy          as BL
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.Encoding       as TLE
import           Data.Void                      ( Void
                                                , absurd
                                                )
import           System.FilePath                ( takeFileName )
import           Test.Tasty
import           Test.Tasty.Golden
import qualified Text.Megaparsec               as MP
import qualified Text.Show.Pretty              as Pretty
import qualified Text.Blaze.Html.Renderer.Text as HT

-- Instances for pretty printing
instance (Pretty.PrettyVal a, Pretty.PrettyVal b) => Pretty.PrettyVal (Map.Map a b) where
  prettyVal m = Pretty.Con "fromList" [m']
    where m' = Pretty.prettyVal $ Map.toList m
instance Pretty.PrettyVal MP.SourcePos
instance Pretty.PrettyVal MP.Pos where
  prettyVal p = Pretty.Con "Pos" [Pretty.prettyVal $ MP.unPos p]
instance Pretty.PrettyVal i => Pretty.PrettyVal (Set.Set i) where
  prettyVal m = Pretty.Con "fromList" [m']
    where m' = Pretty.prettyVal $ Set.toList m
instance Pretty.PrettyVal Void where
  prettyVal = absurd

instance Pretty.PrettyVal SP.Doc
instance Pretty.PrettyVal SP.Attrs
instance Pretty.PrettyVal SP.SecNode

instance Pretty.PrettyVal SP.SecHeader
instance Pretty.PrettyVal SP.BlockNode
instance Pretty.PrettyVal SP.InlineNode
instance Pretty.PrettyVal SP.InlineContent
instance (Pretty.PrettyVal t, Pretty.PrettyVal c) => Pretty.PrettyVal (SP.Element t c)
instance Pretty.PrettyVal SP.BlockContent

instance Pretty.PrettyVal SI.Node
instance Pretty.PrettyVal SI.Element
instance Pretty.PrettyVal SI.Meta
instance Pretty.PrettyVal SI.SourcePresentation

instance Pretty.PrettyVal SC.ContainerName
instance Pretty.PrettyVal SC.CounterName

instance Pretty.PrettyVal SM.Doc
instance Pretty.PrettyVal SM.DocAttrs
instance Pretty.PrettyVal SM.SectionContent
instance Pretty.PrettyVal i => Pretty.PrettyVal (SM.Title i)
instance Pretty.PrettyVal SM.TitlingConfig
instance Pretty.PrettyVal SM.NumberStyle
instance Pretty.PrettyVal SM.Block
instance Pretty.PrettyVal SM.Section
instance Pretty.PrettyVal i => Pretty.PrettyVal (SM.Inline i)
instance Pretty.PrettyVal SM.FormalConfig
instance Pretty.PrettyVal SM.SectionConfig
instance Pretty.PrettyVal SM.Formal
instance Pretty.PrettyVal SM.Paragraph
instance Pretty.PrettyVal SM.TitleParts
instance Pretty.PrettyVal SM.List
instance Pretty.PrettyVal i => Pretty.PrettyVal (SM.Emph i)
instance Pretty.PrettyVal i => Pretty.PrettyVal (SM.Quote i)
instance Pretty.PrettyVal SM.Str
instance Pretty.PrettyVal SM.InlineMath
instance Pretty.PrettyVal SM.DisplayMath
instance Pretty.PrettyVal SM.InlineCode
instance Pretty.PrettyVal SM.PageMark
instance Pretty.PrettyVal SM.Varied
instance Pretty.PrettyVal SM.MixedBlockBody
instance Pretty.PrettyVal SM.ParContent
instance Pretty.PrettyVal SM.VariedVar
--

parseOrExplode :: Text -> Text -> SP.Doc
parseOrExplode name t = case SP.parseDoc' name t of
  Left  e -> error $ T.unpack e
  Right a -> a

-- with decoration
markupOrExplode :: SI.Node -> SM.Doc
markupOrExplode n = case SM.parseDoc n of
  Left  e -> error $ T.unpack $ SM.prettyScribaError e
  Right a -> SD.decorate a

byteShow :: Pretty.PrettyVal a => a -> BL.ByteString
byteShow = TLE.encodeUtf8 . TL.pack . Pretty.valToStr . Pretty.prettyVal

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
