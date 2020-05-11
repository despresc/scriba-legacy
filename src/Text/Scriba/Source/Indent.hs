{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Scriba.Source.Indent where

import           Text.Scriba.Source.Common

import           Control.Applicative            ( (<|>)
                                                , empty
                                                , Alternative
                                                , liftA2
                                                )
import           Control.Monad                  ( MonadPlus )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , lift
                                                )
import qualified Control.Monad.Reader          as Reader
import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                )
import           Data.Functor                   ( void
                                                , ($>)
                                                )
import           Data.String                    ( IsString(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( Parsec
                                                , ParseErrorBundle
                                                , (<?>)
                                                , SourcePos
                                                )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char.Lexer    as MPL

{-

Some kind of better lexical structure?

`#` at the beginning of the line must be the start of a section

-}

-- * Parsing


newtype Parser a = Parser
  { getParser :: ReaderT Int (Parsec Void Text) a
  } deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MP.MonadParsec Void Text, MonadFail)

runParser :: Parser a -> Int -> Parsec Void Text a
runParser = runReaderT . getParser

instance (a ~ Text) => IsString (Parser a) where
  fromString = Parser . lift . fromString

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

type ParseError = ParseErrorBundle Text Void

getIndent :: Parser Int
getIndent = Parser Reader.ask

atIndent :: Int -> Parser a -> Parser a
atIndent n = Parser . Reader.local (const n) . getParser

indentNoNewline :: Parser (Int, Text)
indentNoNewline = do
  t <- MP.takeWhileP Nothing (' ' ==)
  pure (T.length t, t)

-- | Parse a newline and all subsequent space characters.
indent :: Parser (Int, Text)
indent = do
  void "\n"
  indentNoNewline

splitIndent :: Int -> Text -> Parser Text
splitIndent n = pure . T.drop n

-- | Parse a newline, then skip the ambient indentation level, and
-- return the newline and any subsequent excess indent.

-- TODO: make use of the built-in indentation error.
-- TODO: this uses a dummy tab width.
indentText :: Parser Text
indentText = do
  (n, t) <- indent
  lvl    <- getIndent
  case n `compare` lvl of
    LT ->
      fail $ "insufficient indent: got " <> show n <> ", expected " <> show lvl
    _ -> do
      t' <- splitIndent lvl t
      pure $ "\n" <> t'


-- TODO: duplication with indentText
indentTextNoNewline :: Parser Text
indentTextNoNewline = do
  (n, t) <- indentNoNewline
  lvl    <- getIndent
  case n `compare` lvl of
    LT ->
      fail $ "insufficient indent: got " <> show n <> ", expected: " <> show lvl
    _ -> do
      t' <- splitIndent lvl t
      pure $ "\n" <> t'

-- | Parses one or more whitespace characters other than a newline.

pLineSpace1 :: Parser Text
pLineSpace1 = MP.takeWhile1P (Just "line space") $ \c -> isSpace c && c /= '\n'

pLineSpace :: Parser Text
pLineSpace = MP.takeWhileP (Just "line space") $ \c -> isSpace c && c /= '\n'

pBlankLine :: Parser Text
pBlankLine = do
  void "\n"
  t <- pLineSpace
  void $ MP.lookAhead "\n"
  pure $ "\n" <> t

-- TODO: Important! Should the comment parser be aware of the ambient
-- indentation? Test, at least, that nothing is messed up by not
-- having such an awareness. E.g. I would hope that a comment spanning
-- more than one line would always work out.
scLineSpace :: Parser ()
scLineSpace =
  MPL.space (void pLineSpace1) empty (void $ pCommentStart >> pCommentBody)

-- ** Comments and whitespace

-- | Parses a literal @|@, the start of an inline body for block and
-- inline elements. Not lexemic.
pInlineBodyStart :: Parser Text
pInlineBodyStart = "|" <?> "start of inline body"

-- | Parses a literal @&@, the start of a paragraphed body for block
-- elements. Not lexemic.
pBlockBodyStart :: Parser Text
pBlockBodyStart = "---" <?> "start of block body"

-- | Parses a literal @`@, the start of a verbatim body.
pVerbatimBodyStart :: Parser Text
pVerbatimBodyStart = "`" <?> "start of verbatim body"

-- | Parses a literal @`@, the end of a verbatim body.
pVerbatimBodyEnd :: Parser Text
pVerbatimBodyEnd = "`" <?> "end of verbatim body"

pCommentStart :: Parser Text
pCommentStart = "{%" <?> "start of comment"

pCommentEnd :: Parser Text
pCommentEnd = "%}" <?> "end of comment"

pBlockMark :: Parser ()
pBlockMark = void "&" <?> "block marker"

-- | Parses a literal @\@@, the start of the arguments of an
-- element. Not lexemic.
pElementArgsStart :: Parser Text
pElementArgsStart = "@" <?> "start of arguments"

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

-- | An element type is a sequence of alphanumeric characters,
-- underscores, and periods. This may be changed in future. Not
-- lexemic.
pElemTy :: Parser Text
pElemTy = MP.takeWhile1P Nothing isTyChar <?> "element type"
  where isTyChar x = isAlphaNum x || x == '_' || x == '.'

-- | Parses the recognized backslash tokens in plain text. These are
-- @\\@ followed by one of @{}\\@ standing for that following
-- character, and @\\@ not before one of these, standing for @\\@.
pBackslashToks :: Parser Text
pBackslashToks = MP.single '\\' >> MP.option "\\" pEscChar
  where pEscChar = fmap T.singleton $ MP.satisfy $ \c -> T.any (== c) "{}\\"

pInlineTextLine :: Parser Text
pInlineTextLine =
  MP.label "inline text"
    $   fmap T.concat
    $   MP.some
    $   insigChar
    <|> pBackslashToks
  where insigChar = MP.takeWhile1P Nothing $ \c -> not $ T.any (== c) "\\{}\n"

{-
-- Parse a newline and subsequent blank lines, as long as those blank
-- lines are followed by a properly-indented element.
pIndentText :: Parser Text
pIndentText = MP.try $ do
  ls <- MP.many $ MP.eitherP blankLine indentText
  void "\n"
  void $ MP.many 
  where
    blankLine = MP.try $ "\n" >> pLineSpace >> MP.notFollowedBy 
-}


-- | Parse the indent and subsequent whitespace of a line, as long as
-- the line is not entirely blank.
pParaIndent :: Parser Text
pParaIndent = MP.try $ do
  t  <- indentText
  t' <- pLineSpace
  MP.notFollowedBy (void "\n" <|> MP.eof)
  pure $ t <> t'

-- | Parse indented paragraph text.
pParaText :: Parser Text
pParaText =
  MP.label "paragraph text"
    $   fmap T.concat
    $   MP.some
    $   pInlineTextLine
    <|> pLineSpace1
    <|> pParaIndent

-- | Parse indented inline text.
pInlineText :: Parser Text
-- TODO: tries?
-- TODO: duplication
pInlineText =
  MP.label "inline text"
    $   fmap T.concat
    $   MP.some
    $   pInlineTextLine
    <|> blankThenText
 where
  blankLine = MP.try $ do
    void "\n"
    t <- pLineSpace
    void $ MP.lookAhead "\n"
    pure $ "\n" <> t
  blankThenText = MP.try $ do
    ts <- MP.many blankLine
    t  <- indentText
    pure $ T.concat $ ts <> [t]

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

-- TODO: make aware of indent!
-- TODO: duplication with blankLine
-- TODO: could combine blankLine and indentText branches by using indentTextNoNewline
pInlineVerbText :: Parser Text
pInlineVerbText =
  fmap T.concat
    $   MP.many
    $   pInsig
    <|> pBacktickEsc
    <|> pInsigBacktick
    <|> blankLine
    <|> indentText
 where
  pInsig =
    MP.takeWhile1P Nothing (\c -> c /= '`' && c /= '\n')
      <?> "non-backtick characters"
  pBacktickEsc = MP.chunk "``" $> "`"
  pInsigBacktick =
    MP.label "'`' not before '}'" $ MP.try $ MP.chunk "`" <* MP.notFollowedBy
      (MP.single '}')
  blankLine = MP.try $ do
    void "\n"
    t <- pLineSpace
    void $ MP.lookAhead "\n"
    pure $ "\n" <> t

-- | Parses something between the start and end inline element
-- braces. Also takes a description of what the thing is, for
-- labelling the braces.
pBraced :: String -> Parser a -> Parser a
pBraced t = MP.between ("{" <?> "start of " <> t) ("}" <?> "end of " <> t)

-- Assumes it starts at a newline. Consumes blank lines, stopping at a
-- non-empty line that has been indented by at least the ambient
-- indent level.
-- TODO: Not great
pBlankLines :: Parser ()
pBlankLines = void $ MP.many $ MP.try $ do
  void "\n"
  scLineSpace
  MP.lookAhead (void "\n" <|> MP.eof)

-- This consumes all blank lines until just before a line with
-- non-whitespace content in it (or the end of input), then consumes
-- an optional newline. Thus this will stop at the beginning of a
-- line, with that line's indent intact (or at the end of the file).
-- TODO: so many names

-- TODO: maybe we should change the whitespace model slightly? Say for
-- block nodes, maybe we should call (this >> indentText(NoNewline))
-- at the *beginning* of block attribute parsing. Essentially we want
-- to have a many-type combinator that takes care of indentation for
-- us.
pBlockNodeSep :: Parser ()
pBlockNodeSep = scLineSpace >> pBlankLines >> void (MP.optional "\n")

-- TODO: try required?
pSpace :: Parser ()
pSpace = void $ MP.many scBlankLine >> MP.optional (indentText >> scLineSpace)
 where
  scBlankLine = MP.try $ do
    void "\n"
    scLineSpace
    MP.lookAhead "\n"

pInlineAttrs :: Parser space -> Parser [Element Text InlineContent]
pInlineAttrs sc = MP.many $ do
  sp <- MP.getSourcePos
  a  <- pBraced "inline attribute" $ pInlineElementTyped <*> pure sp
  void sc
  pure a

-- | Parse the attributes of a block element. The @space@ parser is used to
-- consume space after each attribute.
pAttrs :: Parser space -> Parser Attrs
pAttrs sc = MP.label "attributes" $ do
  void sc
  inls <- pInlineAttrs sc
  blks <- MP.many $ do
    sp <- MP.getSourcePos
    let ilvl = MP.unPos $ MP.sourceColumn sp
    a <- pBlockElement ilvl pElemTy <*> pure sp
    void sc
    pure a
  pure $ Attrs inls blks

-- | Parse the arguments of an element, which is a sequence of
-- argument nodes starting with a @\@@ marker.
pArgs :: Parser space -> Parser [InlineNode]
pArgs sc = pElementArgsStart >> sc >> MP.many (pArgNode <* sc)
  where pArgNode = pInlineNodeWith pArgText

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

pInlineElementTyped :: Parser (SourcePos -> Element Text InlineContent)
pInlineElementTyped = pElement pSpace pElemTy pInlineContent

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

-- | Parse a block element. The passed @Int@ is the indentation of the
-- block mark.

-- TODO: need to think about the whitespace consumers allowed between
-- all the parts.
-- TODO: inlined pAttrs for now.
-- TODO: restore inline attributes and args.
pBlockElement
  :: Int -> Parser ty -> Parser (SourcePos -> Element ty BlockContent)
pBlockElement ilvl pTy = (pBlockMark >>) $ atIndent ilvl $ do
  scLineSpace
  mty <- pTy
  scLineSpace
  pTight mty <|> pLoose mty
 where
  pContent = MP.option BlockNil $ do
    void indentTextNoNewline
    pBlockParContent <|> pBlockUnparContent <|> pBlockVerbContent
  pTight mty = MP.try $ do
    c <-
      pBlockUnparContent
      <|> pBlockVerbContent
      <|> (MP.notFollowedBy indentText $> BlockNil)
    pure $ \s -> Element s mty (Attrs [] []) [] c
  pLoose mty = do
    pBlockNodeSep
    attrs' <- MP.label "attributes" $ do
      -- inls <- pInlineAttrs sc
      blks <- MP.many $ MP.try $ do
        void indentTextNoNewline
        sp <- MP.getSourcePos
        let ilvl' = MP.unPos $ MP.sourceColumn sp
        a <- pBlockElement ilvl' pElemTy <*> pure sp
        pBlockNodeSep
        pure a
      pure $ Attrs [] blks
    con <- pContent
    pure $ \s -> Element s mty attrs' [] con

-- | Parses paragraphed block content

-- TODO: the optional pSpace is bad. Should we just make the beginning
-- newline optional in pSpace?
-- TODO: use pBlockNodes
pBlockParContent :: Parser BlockContent
pBlockParContent = do
  void pBlockBodyStart
  pBlockNodeSep
  BlockBlocks <$> MP.many (pBlockNode <* pBlockNodeSep)

-- TODO: can I just use the inline seq content parser from inline element?
pBlockUnparContent :: Parser BlockContent
pBlockUnparContent = BlockInlines <$> pSeq
  where pSeq = pInlineBodyStart >> MP.many (pInlineNodeWith pInlineText)

-- | Parses verbatim block content

-- TODO: document whitespace handling
pBlockVerbContent :: Parser BlockContent
pBlockVerbContent = do
  src <- MP.getSourcePos
  void pVerbatimBodyStart
  scLineSpace
  t <- pBlockVerbText
  pure $ BlockVerbatim src $ stripNewline t
  where stripNewline t = maybe t snd $ T.uncons t

-- TODO: duplication with blankLine?
-- TODO: any of these tries necessary?
pBlockVerbText :: Parser Text
pBlockVerbText = fmap T.concat $ MP.many blankThenText
 where
  blankLine = MP.try $ do
    void "\n"
    t <- pLineSpace
    void $ MP.lookAhead "\n"
    pure $ "\n" <> t
  blankThenText = MP.try $ do
    ts   <- MP.many blankLine
    t    <- indentText
    rest <- MP.takeWhileP Nothing (/= '\n')
    pure $ T.concat $ ts <> [t, rest]

-- TODO: for now, block nodes require at least a leading newline to be
-- parsed properly. This forbids regular block nodes from appearing at
-- the very start of a document. Might want to change that. We could
-- instead change the block node separation strategy so that we will
-- have already consumed the newline.

-- TODO: It might also be wiser to have combinators to parse indented
-- space of different kinds, with callbacks for sufficient and
-- insufficient indent. This could be used for a parser that would
-- parse some (or many) of a form, then call a particular callback if
-- the subsequent indentation is sufficient. The sufficient indent
-- case would leave the input at a non-whitespace character, and the
-- insufficient indent case would leave the input at the start of the
-- line (or at the newline before the line).
pBlockNode :: Parser BlockNode
pBlockNode = do
  void indentTextNoNewline
  src <- MP.getSourcePos
  let indentLvl = MP.unPos $ MP.sourceColumn src
  bn <-
    ((BlockBlock .) <$> pBlockElement indentLvl (MP.optional pElemTy)) <|> pPar
  pure $ bn src
  where pPar = pParagraph >>= \p -> pure (\src -> BlockPar src p)

pParagraph :: Parser [InlineNode]
-- TODO: add the paragraph header form.
pParagraph = MP.some pParaNode where pParaNode = pInlineNodeWith pParaText

-- | A section header is a sequence of one or more number signs, then
-- an element type and attributes.

-- TODO: duplication with BlockElement
pSecHeader :: Parser (SourcePos -> SecHeader)
pSecHeader = do
  n <- pNumberRun
  scLineSpace
  mty <- MP.optional pElemTy
  scLineSpace
  pSecBlockSep
  attrs' <- MP.label "attributes" $ do
    -- inls <- pInlineAttrs sc
    blks <- MP.many $ MP.try $ do
      void indentTextNoNewline
      sp <- MP.getSourcePos
      let ilvl' = MP.unPos $ MP.sourceColumn sp
      a <- pBlockElement ilvl' pElemTy <*> pure sp
      pSecBlockSep
      pure a
    pure $ Attrs [] blks
  scLineSpace >> void "\n" <|> MP.eof
  pure $ \s -> SecHeader n s mty attrs' []
 where
  pSecBlockSep = void $ scLineSpace >> MP.optional "\n"
  pNumberRun   = (T.length <$> MP.takeWhile1P Nothing (== '#')) <* scLineSpace

{-
pSecHeader :: Parser (SourcePos -> SecHeader)
pSecHeader = do
  n <- pNumberRun
  let toHeader (Element s t at ar ()) = SecHeader n s t at ar
  f <- pElement pAtMostOneNewline (MP.optional pElemTy) (pure ())
  -- TODO: I _think_ I should enforce a newline.
  -- TODO: Here we see that we should be aware of eof issues with whitespace.
  -- Make sure that enforced whitespace allows eof where possible.
  void pBlankLine <|> MP.eof
  pure $ toHeader . f
 where

  pAtMostOneNewline = pLineSpace >> MP.optional ("\n" >> pLineSpace)
-}

-- TODO: simply have section headers be a type of block node?
pSecContent :: Parser [SecNode]
pSecContent = MP.many $ pSecNode <* pBlockNodeSep

pBlockNodes :: Parser [BlockNode]
pBlockNodes = MP.many (pBlockNode <* pBlockNodeSep)

pSecBlock :: Int -> Parser (SourcePos -> BlockNode)
-- TODO: somewhat duplicates pBlockNode
pSecBlock indentLvl =
  ((BlockBlock .) <$> pBlockElement indentLvl (MP.optional pElemTy)) <|> pPar
  where pPar = pParagraph >>= \p -> pure (\src -> BlockPar src p)

-- | A section node is either a block node or a section header

-- TODO: This assumes that section nodes occur at zero indent!
pSecNode :: Parser SecNode
pSecNode = do
  src <- MP.getSourcePos
  let indentLvl = subtract 1 . MP.unPos $ MP.sourceColumn src
  f <- (SecHeaderNode .) <$> pSecHeader <|> (SecBlock .) <$> pSecBlock indentLvl
  pure $ f src

-- ** Full document

-- TODO: don't just discard the body of the scriba element. Should
-- warn if it's present!
-- TODO: should probably have a more lax "block element parser" to
-- which various component parsers are passed. Certainly would be
-- better than this.
pDocAttrs :: SourcePos -> Parser Attrs
pDocAttrs sp = MP.label "scriba element (document meta)" $ MP.try $ do
  Element _ ty attrs' _ _ <- pBlockElement 1 pElemTy <*> pure sp
  case ty of
    "scriba" -> pure attrs'
    _        -> empty

pDoc :: Parser Doc
pDoc = do
  src <- MP.getSourcePos
  pBlockNodeSep
  at <- MP.option (Attrs [] []) (pDocAttrs src)
  pSpace
  c <- pSecContent
  MP.eof
  pure $ Doc src at c

-- * Parsing

-- TODO: the indent is set to zero. Might want a version where that is
-- not the case.

parseWithAt
  :: Parser a -> Text -> Int -> Text -> Either (ParseErrorBundle Text Void) a
parseWithAt p fn n = MP.parse (runParser p n) (T.unpack fn)

parseWithAt' :: Parser a -> Text -> Int -> Text -> Either Text a
parseWithAt' p fn n =
  either (Left . T.pack . MP.errorBundlePretty) Right . parseWithAt p fn n

parseWith :: Parser a -> Text -> Text -> Either (ParseErrorBundle Text Void) a
parseWith p fn = parseWithAt p fn 0

parseWith' :: Parser a -> Text -> Text -> Either Text a
parseWith' p fn =
  either (Left . T.pack . MP.errorBundlePretty) Right . parseWith p fn

parseBlocks' :: Text -> Text -> Either Text [BlockNode]
parseBlocks' = parseWith' (MP.many (pBlockNode <* pBlockNodeSep) <* MP.eof)

parseDoc' :: Text -> Text -> Either Text Doc
parseDoc' = parseWith' pDoc

-- TODO: temporary
parseTesting :: Parser a -> Int -> Text -> Either Text (Text, a)
parseTesting p inp = parseWithAt' go "<test>" inp
 where
  go = do
    a <- p
    s <- MP.getParserState
    pure (MP.stateInput s, a)


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

    &codeBlock`
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

-- | Parses (and skips) insignificant whitespace characters.
pSpace :: Parser ()
pSpace = MPL.space MP.space1 empty empty


-- | Parse a newline, and subsequent indent. Only considers space
-- indentation. Tab indentation may come later.
pIndent :: Parser Int
pIndent = do
  MP.eol
  t <- MP.takeWhileP Nothing (== ' ')
  pure $ T.length t

-- ** Components of elements


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




-- | An inline verbatim body is a sequence of characters after a
-- single backtick character @`@. In detail, the sequence of
-- characters are any character other than a backtick, a double
-- backtick @``@ standing for a single backtick, or a backtick not
-- followed by a close brace @}@. The sequence stops at the token @`}@
-- or the end of input.

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



-- ** Block elements

pBlockNode :: Parser BlockNode
pBlockNode = do
  src <- MP.getSourcePos
  let indentLvl = subtract 1 . MP.unPos $ MP.sourceColumn src
  bn <- ((BlockBlock .) <$> pBlockElement indentLvl) <|> pPar
  pure $ bn src
  where pPar = pParagraph >>= \p -> pure (\src -> BlockPar src p)



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
