{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Library where

import           Text.Scriba.Markup             ( parseToNode
                                                , nodeToGathered
                                                , gatheredToUnfolded
                                                , GatherData(..)
                                                , prettyDecorateError
                                                , Node
                                                , Identifier
                                                , LinkDatum
                                                , StandaloneConfig(..)
                                                , writeStandalone
                                                , Identifier(..)
                                                , UnfoldedDoc
                                                , UnfoldedNotes
                                                )

import qualified Data.List                     as List
import           Data.Map                       ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Text.Lazy.IO             as LText
import           Development.Shake
import           Development.Shake.FilePath
import qualified Text.Blaze.Html.Renderer.Text as HT

{-

- A simple library configuration is a directory of documents, each
  with a manifest file in an easily-discoverable place. The manifest
  would identify the main file of the document (if present) and
  probably include the document metadata and other properties. Even
  just package-name.<some suffix> or manifest.scb. Something like
  that.

- Are documents expected to list their dependencies explicitly? If
  they're supposed to be versioned separately, then that's probably a
  good idea. On the other hand, that _is_ pretty
  un-ergonomic. Certainly the library itself can't have a version,
  other than possibly for major structural things. But if the library
  is the major unit of communication then it has to be that way! I
  suppose there could simply be moderately infrequent updates? A
  once-a-month sort of thing. Okay.

- So the library is completely release-agnostic and should build,
  precisely and reproducibly, what it is given.

- Maybe I should instead allow doc versioning? It would be easier for
  the library structure, I suppose. Should allow importing common
  styles and configurations!

-}

{- Library data flow:

- go over each document in docs:
  - compile its metadata

  - compile the document up to the linking stage, collecting its
    expected and required linkage information

  - gather the needed linkage information (must be tolerant toward
    missing documents), fill in document references, collect broken
    references and links (should generate identifiers for broken
    reference elements that don't have them)

  - render the documents to HTML

- gather the broken reference/link information and render a report
  page on broken links.

-}

data LinkageInfo = LinkageInfo
  { selfLinkage :: Map Identifier LinkDatum
  , restLinkage :: Map Identifier (Map Identifier LinkDatum)
  }

{-
- Require that the linkage and html information be built for each
  document

- linkage requires that the intermediate.json file be built (kind of
  phony, since the intermediate.json rule will build a number of
  things, including the linkage and doc-linkage-requirements files)

- html information requires that intermediate.json and linkage.json be
  built, as well as doc-linkage-requirements and all the linkage
  information contained in that file, and uses that to render the html
  file (could also render the other outputs formats here, too, and
  just have the top-level action require those outputs, and satisfy
  them through phony dependencies like the linkage information)

- intermediate.json requires that src-include-list and all of the
  files in it are present

- src-include-list (for now!) checks if docs/*/src is present, and
  adds all the files in it to the list if so (it should also remove
  the main file, in fact)

- intermediate.json requires that all the source files are present (I
  suppose by parsing the meta file?), then builds the document up to
  the stage when linking is required.

Probably want functions like readFileLines that require the files be
present then parse them in some way. Like readMetaFile, but actually
filled in.

Need to have better error handling here. Not just calling
`error`. Really the whole thing needs to be more tolerant of failure
and degraded build conditions.

readFile and locale issues?
https://github.com/ndmitchell/shake/issues/756
I think I want a uniform utf-8 encoding for everything, so that's
relevant.

TODO:

need to generate an unfolded-doc.json, and have html require it. It
requires that intermediate.json and the linkage documents listed in
doc-linkage-requirements be present.

-}

{-
TODO:
Gathering and referencing:

- All link targets should report the generated url fragment of their
  page. In each document we get a map (Identifier -> LinkDatum).

- All documents should report the pages that they define (by
  fragment). Assuming we know the base path of the library, the page
  url will be `<base-path>/<doc-name>/<page-name>`. I assume there's a
  way to track that information in the build system via the shake
  cache.

- All references are of the form {link|#thing} or
  {link|#thing.other}. The former does not require any other documents
  be present. The latter requires that document <thing> be present (in
  reality it should fail gracefully if <thing> doesn't exist, with
  some kind of rendered placeholder describing the error).

- from the referenced documents (and the document itself), we can
  build a (Map RefTarget LinkDatum) that we can use to fill in the
  document.
-}

buildLib :: IO ()
buildLib = shakeArgs shakeOptions { shakeFiles = buildDir } $ do
  action $ do
    docs <- getDirectoryDirs scribaSrcDir
    need $ do
      docName <- docs
      t       <- ["linkage", "html" </> "index.html"]
      pure $ inBuildDir docName </> t

  inBuildDir "*/linkage" %> \out -> do
    let docName = getBuildDocName out
    need [inBuildDir docName </> "intermediate"]

  inBuildDir "*/html/index.html" %> \out -> do
    let
      docName = getBuildDocName out
      docDir  = inBuildDir docName
      readUnfoldedDoc f = liftIO $ read . Text.unpack <$> Text.readFile f
      writeHtmlFile f = liftIO . LText.writeFile f . HT.renderHtml
      renderStandaloneHtml notes d =
        writeStandalone (StandaloneConfig "") (notes, d)
    need [docDir </> "unfolded-doc"]
    (d, notes) <- readUnfoldedDoc $ docDir </> "unfolded-doc"
    let t = renderStandaloneHtml (notes :: UnfoldedNotes) (d :: UnfoldedDoc)
    writeHtmlFile (docDir </> "html" </> "index.html") t

  inBuildDir "*/unfolded-doc" %> \out -> do
    let docName = getBuildDocName out
        docDir  = inBuildDir docName
        writeUnfoldedDoc f (LinkageInfo self rest) i = liftIO $ do
          let i' =
                either (error . Text.unpack . prettyDecorateError) id
                  $ gatheredToUnfolded self rest i
          Text.writeFile f (Text.pack $ show i')
        getLinkInfo f fs = do
          self <- read . Text.unpack <$> liftIO (Text.readFile f)
          rest <- forP fs $ \f' ->
            (,) (Identifier $ Text.pack f') . read . Text.unpack <$> liftIO
              (Text.readFile $ buildDir </> f' </> "linkage")
          pure $ LinkageInfo self (Map.fromList rest)
        readIntermediate f = liftIO $ read . Text.unpack <$> Text.readFile f
    otherReqs <- readFileLines $ docDir </> "doc-linkage-requirements"
    let otherReqs' = [ inBuildDir t </> "linkage" | t <- otherReqs ]
    need $ (docDir </> "linkage") : otherReqs'
    linkInfo <- getLinkInfo (docDir </> "linkage") otherReqs
    i        <- readIntermediate $ docDir </> "intermediate"
    writeUnfoldedDoc (docDir </> "unfolded-doc") linkInfo i

  inBuildDir "*/doc-linkage-requirements" %> \out -> do
    let docName = getBuildDocName out
    need [inBuildDir docName </> "intermediate"]

  inBuildDir "*/intermediate" %> \out -> do
    let docName    = getBuildDocName out
        includeDir = inBuildDir docName </> "scribaIncludes"
        mainname   = "main-syntax"
        mainfile   = inBuildDir docName </> mainname
        readSyntaxFile :: FilePath -> Action Node
        readSyntaxFile f = liftIO $ do
          t <- Text.readFile f
          pure $ read $ Text.unpack t
        renderIntermediate (_, n) _ =
          let (d, gd) = either (error . Text.unpack . prettyDecorateError) id
                $ nodeToGathered n
          in  ( (d, gatherNoteText gd)
              , gatherLinkData gd
              , Set.toList $ gatherReferencedDocs gd
              )
        writeIntermediate f d = liftIO $ do
          let t = Text.pack $ show d
          Text.writeFile f t
        writeLinkage f m = liftIO $ do
          let t = Text.pack $ show m
          Text.writeFile f t
        writeLinkReqs f m = liftIO $ do
          let t = Text.unlines $ getIdentifier <$> m
          Text.writeFile f t
    srcIncludes <- readSourceIncludes docName
    need
      $ (inBuildDir docName </> "main-syntax")
      : ((includeDir </>) <$> srcIncludes)
    mainfile'    <- (,) mainname <$> readSyntaxFile mainfile
    srcIncludes' <- forP srcIncludes
      $ \f -> (,) f <$> readSyntaxFile (includeDir </> f)
    let (intermediate, linkage, linkReqs) =
          renderIntermediate mainfile' srcIncludes'
    writeIntermediate (inBuildDir docName </> "intermediate") intermediate
    writeLinkage (inBuildDir docName </> "linkage") linkage
    writeLinkReqs (inBuildDir docName </> "doc-linkage-requirements") linkReqs

  inBuildDir "*/src-include-list" %> \out -> do
    let docName = getBuildDocName out
        srcDir  = scribaSrcDir </> docName </> "src/"
    b  <- doesDirectoryExist srcDir
    cs <- if b then getDirectoryContents srcDir else pure []
    writeFileLines (inBuildDir docName </> "src-include-list")
      $   ((inBuildDir docName </> "scribaIncludes") </>)
      <$> cs

  inBuildDir "*/main-syntax" %> \out -> do
    let docName  = getBuildDocName out
        mainName = scribaSrcDir </> docName </> "index.scb"
        readScribaSourceFile f = liftIO $ do
          t <- Text.readFile f
          pure $ either (error . Text.unpack) id $ parseToNode (Text.pack f) t
        writeScribaSyntaxFile f n = liftIO $ do
          let t = Text.pack $ show n
          Text.writeFile f t
    need [mainName]
    src <- readScribaSourceFile mainName
    writeScribaSyntaxFile (inBuildDir docName </> "main-syntax") src

 where
  buildDir     = "_build"
  scribaSrcDir = "docs"
  inBuildDir   = (buildDir </>)
  getBuildDocName f = splitDirectories f List.!! 1
  -- Dummy implementations
  readSourceIncludes f = readFileLines $ inBuildDir f </> "src-include-list"
