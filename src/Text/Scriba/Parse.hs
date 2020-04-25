{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Parse where

import           Control.Applicative            ( (<|>)
                                                , empty
                                                )
import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                )
import           Data.Functor                   ( void
                                                , ($>)
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , ParseErrorBundle
                                                , (<?>)
                                                , SourcePos
                                                )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP
import qualified Text.Megaparsec.Char.Lexer    as MPL

{- TODO:

- Make sure things are labelled appropriately.

- Go through the documentation. Add module documentation.

- Add a double-backtick verbatim block form that strips indent? Or is
  that redundant?

- Should comments be preserved? If we add a document formatter then
  source comments should probably be left in-place instead of being
  stripped out.

-}

-- * Document syntax

-- | A document has metadata and sectional content.
data Doc = Doc SourcePos Attrs [SecNode]
  deriving (Eq, Ord, Show, Read)

-- | Attributes are the same as a list of typed inline elements with
-- no two types repeated.
newtype Attrs = Attrs
  { getAttrs :: Map Text Attr
  } deriving (Eq, Ord, Show, Read, Semigroup, Monoid)

attrsFromList :: [(Text, Attr)] -> Attrs
attrsFromList = Attrs . M.fromList

type Attr = (SourcePos, Attrs, [InlineNode], InlineContent)

-- | An element has a position in the source, a type, meta, and
-- content.
data Element t c = Element
  { srcPos :: SourcePos
  , etype :: t
  , attrs :: Attrs
  , args :: [InlineNode]
  , content :: c
  } deriving (Eq, Ord, Show, Read)

-- | A section node is either a section header or a block.
data SecNode
  = SecHeaderNode SecHeader
  | SecBlock BlockNode
  deriving (Eq, Ord, Show, Read)

data SecHeader = SecHeader Int SourcePos (Maybe Text) Attrs [InlineNode]
 deriving (Eq, Ord, Show, Read)

-- | A block node is either a block element or a paragraph.
data BlockNode
  = BlockBlock BlockElement
  -- TODO: implement paragraph header syntax?
  | BlockPar SourcePos [InlineNode]
  deriving (Eq, Ord, Show, Read)

-- | A block element has an always-present type and block content.
type BlockElement = Element (Maybe Text) BlockContent

-- | Block content is either a sequence of blocks (normal paragraphed
-- block content), a sequence of inlines (a block of inlines), or
-- verbatim content.
data BlockContent
  = BlockBlocks [BlockNode]
  | BlockInlines [InlineNode]
  | BlockVerbatim SourcePos Text
  | BlockNil
  deriving (Eq, Ord, Show, Read)

-- | Inline sequence content is a list of inline nodes and plain text.
data InlineNode
  = InlineBraced InlineElement
  | InlineText SourcePos Text
  | InlineComment SourcePos Text
  deriving (Eq, Ord, Show, Read)

-- | An inline element has an optional type and inline content.
type InlineElement = Element (Maybe Text) InlineContent

data InlineContent
  = InlineSequence [InlineNode]
  | InlineVerbatim SourcePos Text
  | InlineNil
  deriving (Eq, Ord, Show, Read)

-- * Parsing

type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void

-- ** Comments and whitespace

pCommentStart :: Parser Text
pCommentStart = "{%" <?> "start of comment"

pCommentEnd :: Parser Text
pCommentEnd = "%}" <?> "end of comment"

-- | Parses the body of a nested comment.

-- TODO: should probably accumulate with a snoc list, or a text
-- builder. That might be true of the plain text parsers as well,
-- honestly.
pCommentBody :: Parser Text
pCommentBody = T.concat <$> go 0
 where
  insigChar =
    MP.takeWhileP (Just "comment text") $ \c -> not $ c == '%' || c == '{'
  go :: Int -> Parser [Text]
  go n = do
    t <- insigChar
    e <- MP.optional $ MP.eitherP pCommentEnd pCommentStart
    case (n :: Int, e) of
      -- TODO: perhaps add location of the unbalanced comment start
      -- in an error?
      (_, Nothing) -> do
        c  <- MP.takeP (Just "comment text") 1
        cs <- go n
        pure $ t : c : cs
      (0, Just (Left _) ) -> pure [t]
      (_, Just (Left cb)) -> do
        cs <- go $ n - 1
        pure $ t : cb : cs
      (_, Just (Right cb)) -> do
        cs <- go $ n + 1
        pure $ t : cb : cs

-- | Parses (and skips) insignificant whitespace characters.
pSpace :: Parser ()
pSpace = MPL.space MP.space1 empty empty

-- | A wrapper for lexemes using the 'pSpace' space consumer.
lexemeAny :: Parser a -> Parser a
lexemeAny = MPL.lexeme pSpace

-- | Consumes a sequence of insignificant non-newline whitespace
-- characters, then a single newline. If this is run at the beginning
-- of a line then this succeeds on a genuine blank line.
pBlankLine :: Parser ()
pBlankLine = pLineSpace <* "\n" <?> "blank end of line"

-- | Parses (and skips) insignificant whitespace characters other than
-- a newline.
pLineSpace :: Parser ()
pLineSpace = MPL.space lineSpace empty empty <?> "line space"
 where
  lineSpace =
    void $ MP.takeWhile1P (Just "line space") $ \c -> isSpace c && c /= '\n'

-- | A wrapper for lexemes using the 'pLineSpace' space consumer.
lexemeLine :: Parser a -> Parser a
lexemeLine = MPL.lexeme pLineSpace

-- ** Plain and verbatim text

-- | Inline text is a sequence of backslash sequences and characters
-- other than one of @{}\\@
pInlineText :: Parser Text
-- TODO: is the try required?
pInlineText =
  MP.label "inline text"
    $   fmap T.concat
    $   MP.some
    $   insigChar
    <|> MP.try pBackslashToks
  where insigChar = MP.takeWhile1P Nothing $ \c -> not $ T.any (== c) "\\{}"

-- | Paragraph text is inline text, with the additional restriction
-- that no blank lines be present. The newline character immediately
-- before the blank line is not consumed by this parser.
pParaText :: Parser Text
pParaText =
  MP.label "paragraph text"
    $   fmap T.concat
    $   MP.some
    $   insigChar
    <|> insigNewline
    <|> pBackslashToks
 where
  insigChar    = MP.takeWhile1P Nothing $ \c -> not $ T.any (== c) "\\{}\n"
  -- TODO: is the try required?
  insigNewline = MP.try $ MP.chunk "\n" <* MP.notFollowedBy pBlankLine

-- | Argument text is inline text except that it cannot contain spaces
-- or the @|@ character.
pArgText :: Parser Text
-- TODO: is the try required?
pArgText =
  MP.label "argument text"
    $   fmap T.concat
    $   MP.some
    $   insigChar
    <|> MP.try pBackslashToks
 where
  insigChar =
    MP.takeWhile1P Nothing $ \c -> not $ T.any (== c) "\\{}|" || isSpace c

-- | Parses the recognized backslash tokens in plain text. These are
-- @\\@ followed by one of @{}\\@ standing for that following
-- character, and @\\@ not before one of these, standing for @\\@.
pBackslashToks :: Parser Text
pBackslashToks = MP.single '\\' >> MP.option "\\" pEscChar
  where pEscChar = fmap T.singleton $ MP.satisfy $ \c -> T.any (== c) "{}\\"

-- | An inline verbatim body is a sequence of characters after a
-- single backtick character @`@. In detail, the sequence of
-- characters are any character other than a backtick, a double
-- backtick @``@ standing for a single backtick, or a backtick not
-- followed by a close brace @}@. The sequence stops at the token @`}@
-- or the end of input.
pInlineVerbText :: Parser Text
pInlineVerbText =
  fmap T.concat $ MP.many $ pInsig <|> pBacktickEsc <|> pInsigBacktick
 where
  pInsig       = MP.takeWhile1P Nothing (/= '`') <?> "non-backtick characters"
  pBacktickEsc = MP.chunk "``" $> "`"
  pInsigBacktick =
    MP.label "'`' not before '}'" $ MP.try $ MP.chunk "`" <* MP.notFollowedBy
      (MP.single '}')

-- | A block verbatim is a sequence of text ending with @\n`}@.

-- TODO: possible enhancement: track the indentation of the start of
-- the verbatim block itself, so that the `} can be allowed to line up
-- with it, instead of being constrained to start at the beginning of
-- a line.
pBlockVerbText :: Parser Text
pBlockVerbText = pVerbText <* "\n"
 where
  pVerbText     = fmap T.concat $ MP.many $ pInsig <|> pNewlineInsig
  pInsig        = MP.takeWhile1P Nothing (/= '\n')
  pNewlineInsig = MP.try $ "\n" <* MP.notFollowedBy "`}"

-- ** Components of elements

-- | An element type is a sequence of alphanumeric characters. This
-- may be changed in future. Not lexemic.
pElemTy :: Parser Text
pElemTy = MP.takeWhile1P Nothing isAlphaNum <?> "element type"

-- | Parses a literal @\@@, the start of the arguments of an
-- element. Not lexemic.
pElementArgsStart :: Parser Text
pElementArgsStart = "@" <?> "start of arguments"

-- | Parses a literal @|@, the start of an inline body for block and
-- inline elements. Not lexemic.
pInlineBodyStart :: Parser Text
pInlineBodyStart = "|" <?> "start of inline body"

-- | Parses a literal @&@, the start of a paragraphed body for block
-- elements. Not lexemic.
pBlockBodyStart :: Parser Text
pBlockBodyStart = "&" <?> "start of block body"

-- | Token for the start of a block.
pBlockMark :: Parser ()
pBlockMark = lexemeAny (void "&") <?> "start of paragraphed block"

-- | Parses something between the start and end inline element braces.
pBraced :: Parser a -> Parser a
pBraced = MP.between "{" "}"

-- | Parse the body of an element with the given components. The
-- @space@ parser is used for inter-component space consumption, and
-- the @content@ parser is responsible for consuming any beginning
-- content delimiter, but should not consume the very final content
-- delimiter (usually @}@).
pElement
  :: Parser space
  -> Parser ty
  -> Parser content
  -> Parser (SourcePos -> Element ty content)
pElement sc pTy pCon = do
  t   <- sc *> pTy <* sc
  at  <- pAttrs sc
  ar  <- MP.option [] $ pArgs sc
  con <- pCon
  pure $ \s -> Element s t at ar con

-- | Parse the attributes of an element. The @space@ parser is used to
-- consume space after each attribute.
pAttrs :: Parser space -> Parser Attrs
pAttrs sc = fmap attrsFromList . MP.many . MP.label "attribute" $ do
  src                   <- MP.getSourcePos
  Element s t at ar con <- pAttr <*> pure src
  void sc
  pure (t, (s, at, ar, con))
  where pAttr = pBraced $ pElement pSpace pElemTy pInlineContent

-- | Parse the arguments of an element, which is a sequence of
-- argument nodes starting with a @\@@ marker.
pArgs :: Parser space -> Parser [InlineNode]
pArgs sc = pElementArgsStart >> sc >> MP.many (pArgNode <* sc)
  where pArgNode = pInlineNodeWith pArgText

-- ** Inline elements

-- | Parse an inline node with the given comment and text node
-- parser. The comment parser is passed in separately so that
-- different syntactic forms can process comments and surrounding
-- whitespace differently. See the implementation of 'pParaContent'
-- for an example.
pInlineNodeWith :: Parser Text -> Parser InlineNode
pInlineNodeWith pText = do
  src <- MP.getSourcePos
  eet <- MP.eitherP (pCommentStart >> pCommentBody)
    $ MP.eitherP pInlineElement pText
  pure $ case eet of
    Left  c         -> InlineComment src c
    Right (Left  e) -> InlineBraced $ e src
    Right (Right t) -> InlineText src t

pInlineElement :: Parser (SourcePos -> InlineElement)
pInlineElement = pBraced $ pElement pSpace (MP.optional pElemTy) pInlineContent

pInlineContent :: Parser InlineContent
pInlineContent = MP.option InlineNil $ pInlineSeqBody <|> pInlineVerbBody
 where
  pInlineSeqBody = fmap InlineSequence $ pInlineBodyStart >> MP.many
    (pInlineNodeWith pInlineText)
  pInlineVerbBody = do
    src <- MP.getSourcePos
    -- TODO: better label?
    InlineVerbatim src <$> MP.between "`" "`" pInlineVerbText

-- ** Block elements

pBlockNode :: Parser BlockNode
pBlockNode = do
  src <- MP.getSourcePos
  bn  <- ((BlockBlock .) <$> pBlockElement) <|> pPar
  pure $ bn src
  where pPar = pParagraph >>= \p -> pure (\src -> BlockPar src p)

pParagraph :: Parser [InlineNode]
-- TODO: add the paragraph header form.
pParagraph = MP.some pParaNode where pParaNode = pInlineNodeWith pParaText

pBlockElement :: Parser (SourcePos -> BlockElement)
pBlockElement = pBlockMark
  >> pBraced (pElement pSpace (MP.optional pElemTy) pContent)
 where
  pContent =
    MP.option BlockNil
      $   pBlockParContent
      <|> pBlockUnparContent
      <|> pBlockVerbContent

-- | Parses paragraphed block content
pBlockParContent :: Parser BlockContent
pBlockParContent = do
  void $ pBlockBodyStart
  pSpace
  BlockBlocks <$> MP.many (pBlockNode <* pSpace)

-- | Parses verbatim block content
pBlockVerbContent :: Parser BlockContent
pBlockVerbContent = do
  src <- MP.getSourcePos
  void "`" -- TODO: label?
  pBlankLine
  t <- pBlockVerbText
  void "`"
  pure $ BlockVerbatim src t

pBlockUnparContent :: Parser BlockContent
pBlockUnparContent = BlockInlines <$> pSeq
  where
   -- TODO: reduce duplication with pInlineContent. This is defined
   -- here because we we need block verbatim content to overrule an
   -- inline verbatim content interpretation.
        pSeq = pInlineBodyStart >> MP.many (pInlineNodeWith pInlineText)

-- ** Section elements

pSecContent :: Parser [SecNode]
pSecContent = MP.many $ pSecNode <* pSpace

-- | A section node is either a block node or a section header
pSecNode :: Parser SecNode
pSecNode = do
  src <- MP.getSourcePos
  f   <- (SecHeaderNode .) <$> pSecHeader <|> (SecBlock .) <$> pSecBlock
  pure $ f src

pSecBlock :: Parser (SourcePos -> BlockNode)
-- TODO: somewhat duplicates pBlockNode
pSecBlock = ((BlockBlock .) <$> pBlockElement) <|> pPar
  where pPar = pParagraph >>= \p -> pure (\src -> BlockPar src p)

-- | A section header is a sequence of one or more number signs, then
-- an element type and attributes.
pSecHeader :: Parser (SourcePos -> SecHeader)
pSecHeader = do
  n <- pNumberRun
  let toHeader (Element s t at ar ()) = SecHeader n s t at ar
  f <- pElement pAtMostOneNewline (MP.optional pElemTy) (pure ())
  -- TODO: I _think_ I should enforce a newline.
  -- TODO: Here we see that we should be aware of eof issues with whitespace.
  -- Make sure that enforced whitespace allows eof where possible.
  pBlankLine <|> MP.eof
  pure $ toHeader . f
 where
  pNumberRun        = lexemeLine $ T.length <$> MP.takeWhile1P Nothing (== '#')
  -- TODO: is this right?
  pAtMostOneNewline = pLineSpace >> MP.optional ("\n" >> pLineSpace)

-- ** Full document

-- TODO: duplication, perhaps.
pDocAttrs :: Parser Attrs
pDocAttrs = pBraced $ do
  pSpace
  void $ "scriba"
  pSpace
  pAttrs pSpace

pDoc :: Parser Doc
pDoc = do
  src <- MP.getSourcePos
  pSpace
  at <- MP.option mempty pDocAttrs
  pSpace
  c <- pSecContent
  MP.eof
  pure $ Doc src at c

-- * Running parsers

parseWith :: Parser a -> Text -> Text -> Either (ParseErrorBundle Text Void) a
parseWith p = MP.parse p . T.unpack

parseWith' :: Parser a -> Text -> Text -> Either Text a
parseWith' p fn =
  either (Left . T.pack . MP.errorBundlePretty) Right . parseWith p fn

parseDoc :: Text -> Text -> Either (ParseErrorBundle Text Void) Doc
parseDoc = parseWith pDoc

parseDoc' :: Text -> Text -> Either Text Doc
parseDoc' = parseWith' pDoc
