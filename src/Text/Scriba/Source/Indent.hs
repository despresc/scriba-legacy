{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Scriba.Source.Indent where

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
import           GHC.Generics                   ( Generic )
import           Text.Megaparsec                ( Parsec
                                                , ParseErrorBundle
                                                , (<?>)
                                                , Pos
                                                , SourcePos
                                                )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP
import qualified Text.Megaparsec.Char.Lexer    as MPL

{-

The document starts with an optional scriba block, then is a sequence
of section headers and blocks.

All syntactic elements exist in an indentation context. This
indentation context is zero at the beginning of a document, and a new
context is formed at the start of any syntactic block. The attributes
and content of a block are indented more than the indentation of the
start of the block, and the content ends when text is encountered of
insufficient indentation (or the end of input). Purely whitespace
lines do not count; if one is encountered then its whitespace is
stripped up to the current indentation level. Leading and trailing
whitespace lines are automatically stripped from block content.

Section headers:

# section {id|stuff} {title|Otherstuff}

<content>

and

# section
&id|stuff
&title|otherstuff
---
<content>

Section headers start with <number signs>, then <type>, then a
sequence of <inline element> separated by section attribute
whitespace, then an optional sequence of <block element> followed by a
... line. No indentation context is created.

Blocks start with &, then a type, then inline attributes, then block
attributes, then one of `, |, ---. The --- signals block content, the
| signals inline content, and the ` signals verbatim content. Verbatim
content ends with a ` at the outer indentation context, and block and
inline content when the current context ends. I think we should still
have nix rules for indentation stripping after this, though.

Paragraphs are the other type of block. They start with anything other
than a # or a &, and contain a sequence of

-}

{-

&olist~
  &item|
    Some {emph|interesting} list item
  &item|
    And more.
  &item~
    This one contains a code block illustrating a point:

    &codeBlock `
      Stuff
      more stuff
      and stuff
  &item and even more

&scriba
  &title|
    The scriba markup language and document system
  &type|
    article
  &formula
    &numbering
      &relative@subsection
      &style@decimal
    &ref
      &prefix|
        eqn.
      &sep|
        {` `}

What should the block separator be?

&olist$
  &li|
    Stuff

I could have the attribute block marker be different than the another
marker, I suppose.

&olist
  !id|something
  &li
    Bleh

    Is this worth it?

&olist {id|something}
  ...
  &li
    Bleh

    Is this worth it?

and maybe | for inline still? Fine.

&li
  ---
  What is this?
  Some interesting text

&li {id|thing} |
  Some stuff.

-}

{-

data Doc = Doc SourcePos Attrs [SecNode]
  deriving (Eq, Ord, Show, Read, Generic)

data Attrs = Attrs
  { inlineAttrs :: [Element Text InlineContent]
  , blockAttrs ::  [Element Text BlockContent]
  } deriving (Eq, Ord, Show, Read, Generic)

data SecNode
  = SecHeaderNode SecHeader
  | SecBlock BlockNode
  deriving (Eq, Ord, Show, Read, Generic)

data Element t c = Element
  { srcPos :: SourcePos
  , etype :: t
  , attrs :: Attrs
  , args :: [InlineNode]
  , content :: c
  } deriving (Eq, Ord, Show, Read, Generic)

data InlineContent
  = InlineSequence [InlineNode]
  | InlineVerbatim SourcePos Text
  | InlineNil
  deriving (Eq, Ord, Show, Read, Generic)

type BlockElement = Element (Maybe Text) BlockContent

data BlockContent
  = BlockBlocks [BlockNode]
  | BlockInlines [InlineNode]
  | BlockVerbatim SourcePos Text
  | BlockNil
  deriving (Eq, Ord, Show, Read, Generic)

data SecHeader = SecHeader Int SourcePos (Maybe Text) Attrs [InlineNode]
 deriving (Eq, Ord, Show, Read, Generic)

data BlockNode
  = BlockBlock BlockElement
  | BlockPar SourcePos [InlineNode]
  deriving (Eq, Ord, Show, Read, Generic)

data InlineNode
  = InlineBraced InlineElement
  | InlineText SourcePos Text
  | InlineComment SourcePos Text
  deriving (Eq, Ord, Show, Read, Generic)

type InlineElement = Element (Maybe Text) InlineContent

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

-- | Parses (and skips) insignificant whitespace characters other than
-- a newline.
pLineSpace :: Parser ()
pLineSpace = MPL.space lineSpace empty empty <?> "line space"
 where
  lineSpace =
    void $ MP.takeWhile1P (Just "line space") $ \c -> isSpace c && c /= '\n'

-- ** Plain and verbatim text

-- | Parses the recognized backslash tokens in plain text. These are
-- @\\@ followed by one of @{}\\@ standing for that following
-- character, and @\\@ not before one of these, standing for @\\@.
pBackslashToks :: Parser Text
pBackslashToks = MP.single '\\' >> MP.option "\\" pEscChar
  where pEscChar = fmap T.singleton $ MP.satisfy $ \c -> T.any (== c) "{}\\"

-- | Parse a newline, and subsequent indent. Only considers space
-- indentation. Tab indentation may come later.
pIndent :: Parser Int
pIndent = do
  MP.eol
  t <- MP.takeWhileP Nothing (== ' ')
  pure $ T.length t

-- | Inline text is a sequence of backslash sequences and characters
-- other than one of @{}\\@. This parser recognizes text at the given
-- indentation level.
pInlineText :: Pos -> Parser Text
pInlineText iLvl = MP.label "inline text"
  $ MPL.indentBlock pLineSpace (pure strategy)
 where
  strategy = MPL.IndentSome (Just iLvl)
                            (pure . T.intercalate "\n")
                            (insigChar <|> pBackslashToks)
  insigChar = MP.takeWhile1P Nothing $ \c -> not $ T.any (== c) "\\{}\n"

pInlineTextLine :: Parser [Text]
pInlineTextLine =
  MP.label "inline text" $ MP.some $ insigChar <|> pBackslashToks
  where insigChar = MP.takeWhile1P Nothing $ \c -> not $ T.any (== c) "\\{}\n"

-- ** Components of elements

-- | An element type is a sequence of alphanumeric characters,
-- underscores, and periods. This may be changed in future. Not
-- lexemic.
pElemTy :: Parser Text
pElemTy = MP.takeWhile1P Nothing isTyChar <?> "element type"
  where isTyChar x = isAlphaNum x || x == '_' || x == '.'

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

-- | Parses a literal @`@, the start of a verbatim body.
pVerbatimBodyStart :: Parser Text
pVerbatimBodyStart = "`" <?> "start of verbatim body"

-- | Parses a literal @`@, the end of a verbatim body.
pVerbatimBodyEnd :: Parser Text
pVerbatimBodyEnd = "`" <?> "end of verbatim body"

-- | Parses something between the start and end inline element
-- braces. Also takes a description of what the thing is, for
-- labelling the braces.
pBraced :: String -> Parser a -> Parser a
pBraced t = MP.between ("{" <?> "start of " <> t) ("}" <?> "end of " <> t)

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
pAttrs sc = MP.many . MP.label "attribute" $ do
  src                   <- MP.getSourcePos
  Element s t at ar con <- pAttr <*> pure src
  void sc
  pure (t, (s, at, ar, con))
  where pAttr = pBraced "attribute" $ pElement pSpace pElemTy pInlineContent

-- | Parse the arguments of an element, which is a sequence of
-- argument nodes starting with a @\@@ marker.
pArgs :: Parser space -> Parser [InlineNode]
pArgs sc = pElementArgsStart >> sc >> MP.many (pArgNode <* sc)
  where pArgNode = pInlineNodeWith pArgText


{-

An indentation-sensitive sequence parser should take:

- A parser for the element in question

- a callback for what happens when a lower indentation level occurs


-}
{-
pInlineText :: Parser Text
pInlineText =
  MP.label "inline text"
    $   fmap T.concat
    $   MP.some
    $   insigChar
    <|> pBackslashToks
  where
-}

{-

-- | A wrapper for lexemes using the 'pSpace' space consumer.
lexemeAny :: Parser a -> Parser a
lexemeAny = MPL.lexeme pSpace

-- | Consumes a sequence of insignificant non-newline whitespace
-- characters, then a single newline. If this is run at the beginning
-- of a line then this succeeds on a genuine blank line.
pBlankLine :: Parser ()
pBlankLine = pLineSpace <* "\n" <?> "blank end of line"


-- | A wrapper for lexemes using the 'pLineSpace' space consumer.
lexemeLine :: Parser a -> Parser a
lexemeLine = MPL.lexeme pLineSpace


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
  insigNewline = MP.try $ MP.chunk "\n" <* MP.notFollowedBy pBlankLine

-- | Argument text is inline text except that it cannot contain spaces
-- or a character from @|&`@.
pArgText :: Parser Text
pArgText =
  MP.label "argument text"
    $   fmap T.concat
    $   MP.some
    $   insigChar
    <|> pBackslashToks
 where
  insigChar =
    MP.takeWhile1P Nothing $ \c -> not $ T.any (== c) "\\{}|`&" || isSpace c

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

-- | A block verbatim is a sequence of text ending with the sequence
-- @`}@. In @pBlockVerbText n@, that ending sequence may be indented
-- by as many as @n@ space characters.

-- The parsing behaviour here could become more restrictive, at the
-- cost of more complexity in this parser. We could, for instance, say
-- that the ending token should never occur indented more than the
-- minimum indent of lines that come before it. This would require
-- that we track that quantity while parsing. If we did that then we
-- might as well strip off the common indent from the block, then
-- report the common indent level in the source presentation
-- somewhere.
pBlockVerbText :: Int -> Parser Text
pBlockVerbText indentLvl = pVerbText <* "\n" <* MP.takeWhileP Nothing (== ' ')
 where
  pVerbText     = fmap T.concat $ MP.many $ pInsig <|> pNewlineInsig
  pInsig        = MP.takeWhile1P Nothing (/= '\n')
  pNewlineInsig = MP.try $ "\n" <* MP.notFollowedBy
    (MP.count' 0 indentLvl pSingleSpace >> "`}")
  pSingleSpace = MP.single ' '


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
pInlineElement = pBraced "inline element"
  $ pElement pSpace (MP.optional pElemTy) pInlineContent

pInlineContent :: Parser InlineContent
pInlineContent = MP.option InlineNil $ pInlineSeqBody <|> pInlineVerbBody
 where
  pInlineSeqBody = fmap InlineSequence $ pInlineBodyStart >> MP.many
    (pInlineNodeWith pInlineText)
  pInlineVerbBody = do
    src <- MP.getSourcePos
    -- TODO: better label?
    InlineVerbatim src
      <$> MP.between pVerbatimBodyStart pVerbatimBodyEnd pInlineVerbText

-- ** Block elements

pBlockNode :: Parser BlockNode
pBlockNode = do
  src <- MP.getSourcePos
  let indentLvl = subtract 1 . MP.unPos $ MP.sourceColumn src
  bn <- ((BlockBlock .) <$> pBlockElement indentLvl) <|> pPar
  pure $ bn src
  where pPar = pParagraph >>= \p -> pure (\src -> BlockPar src p)

pParagraph :: Parser [InlineNode]
-- TODO: add the paragraph header form.
pParagraph = MP.some pParaNode where pParaNode = pInlineNodeWith pParaText

-- | Parse a block element. The given @Int@ is the position of the
-- start of the block on the line, to be passed to
-- 'pBlockVerbContent'.
pBlockElement :: Int -> Parser (SourcePos -> BlockElement)
pBlockElement indentLvl = pBlockMark
  >> pBraced "block element" (pElement pSpace (MP.optional pElemTy) pContent)
 where
  pContent =
    MP.option BlockNil
      $   pBlockParContent
      <|> pBlockUnparContent
      <|> pBlockVerbContent indentLvl

-- | Parses paragraphed block content
pBlockParContent :: Parser BlockContent
pBlockParContent = do
  void pBlockBodyStart
  pSpace
  BlockBlocks <$> MP.many (pBlockNode <* pSpace)

-- | Parses verbatim block content
pBlockVerbContent :: Int -> Parser BlockContent
pBlockVerbContent indentLvl = do
  src <- MP.getSourcePos
  void pVerbatimBodyStart
  pBlankLine
  t <- pBlockVerbText indentLvl
  void pVerbatimBodyEnd
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
  let indentLvl = subtract 1 . MP.unPos $ MP.sourceColumn src
  f <- (SecHeaderNode .) <$> pSecHeader <|> (SecBlock .) <$> pSecBlock indentLvl
  pure $ f src

pSecBlock :: Int -> Parser (SourcePos -> BlockNode)
-- TODO: somewhat duplicates pBlockNode
pSecBlock indentLvl = ((BlockBlock .) <$> pBlockElement indentLvl) <|> pPar
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
  pAtMostOneNewline = pLineSpace >> MP.optional ("\n" >> pLineSpace)

-- ** Full document

-- TODO: duplication, perhaps.
pDocAttrs :: Parser Attrs
pDocAttrs = pBraced "scriba element (document meta)" $ do
  pSpace
  void "scriba"
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
-}
-}
