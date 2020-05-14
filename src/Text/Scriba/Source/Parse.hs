{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Scriba.Source.Parse where

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

{- TODO: 

- in future, may want to cut down on the use of
  getSourcePos. E.g. things can return the SourcePos of the form
  immediately after them, for use in the next form. So pSpace would
  return the SourcePos, for instance, and block parsers would pass
  along the SourcePos of their end so that outer blocks could examine
  that, and not need to call getSourcePos.

- have the content markers for blocks create their own indentation
  context?

- we might want to restore the old indentation stripping behaviour for
  inline text. Would combine well with block content markers creating
  their own indentation context. This can be accomplished by something
  like the block verbatim text parser.

- Make the block verbatim text parser and the other parts of the
  language recognize indentation similarly. Block verbatim text only
  recognizes space indentation, while the rest of the language
  recognizes _any_ space character as indentation. Perhaps only ' '
  and '\t' should be indentation? This relates to fixing splitIndent
  (so it recognizes tabs), crlf recognition, and restoring the old
  indentation stripping behaviour.

- Add a paragraph header form?

- Better labels, e.g. for the components of a BlockElement. Right now
  it's a mess: the line

  & codeBlock `

  gives me the error
    unexpected "`<newline> "
    expecting block marker, de-indented content, start of arguments,
    start of block body, start of inline body, start of inline
    element, or start of verbatim body

- Remove the requirement that section attributes must end in a ---, if present?

- Parse regions. I want "when parsing ..." errors.

-}

-- Parser definitions

newtype Parser a = Parser
  { getParser :: ReaderT Int (Parsec Void Text) a
  } deriving ( Functor
             , Applicative
             , Monad
             , Alternative
             , MonadPlus
             , MP.MonadParsec Void Text
             , MonadFail )

runParser :: Parser a -> Int -> Parsec Void Text a
runParser = runReaderT . getParser

instance (a ~ Text) => IsString (Parser a) where
  fromString = Parser . lift . fromString

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

type ParseError = ParseErrorBundle Text Void

-- Basic indent parsers

getIndent :: Parser Int
getIndent = Parser Reader.ask

atIndent :: Int -> Parser a -> Parser a
atIndent n = Parser . Reader.local (const n) . getParser

splitIndent :: Int -> Text -> Parser Text
splitIndent n = pure . T.drop n

-- | Parses one or more whitespace characters other than a newline.

cSpace :: Parser ()
cSpace = void $ MP.takeWhileP Nothing isSpace

cLineSpace :: Parser ()
cLineSpace = void $ MP.takeWhileP Nothing $ \c -> isSpace c && c /= '\n'

-- TODO: try?
pBlankLine :: Parser Text
pBlankLine = MP.label "blank line" $ MP.try $ do
  void "\n"
  t <- pLineSpace
  void $ MP.lookAhead "\n"
  pure $ "\n" <> t

posIndent :: SourcePos -> Int
posIndent = (subtract 1) . MP.unPos . MP.sourceColumn

-- Run the first parser if a de-indent is encountered.
atDeindent :: a -> Parser a -> Parser a
atDeindent low pHigh = do
  sp <- MP.getSourcePos
  let n = posIndent sp
  lvl <- getIndent
  if n < lvl then pure low else pHigh

guardIndented :: Parser ()
guardIndented = do
  sp <- MP.getSourcePos
  let n = posIndent sp
  lvl <- getIndent
  if n < lvl
    then
      fail $ "insufficient indent: got " <> show n <> ", expected " <> show lvl
    else pure ()

-- Document parts

pDoc :: Parser Doc
pDoc = do
  src <- MP.getSourcePos
  cSpace
  at <- MP.option (Attrs [] []) pDocAttrs
  cSpace
  c <- pSecContent
  MP.eof
  pure $ Doc src at c

-- TODO: duplication with BlockElement
pDocAttrs :: Parser Attrs
pDocAttrs = do
  sp <- MP.getSourcePos
  MP.try $ pBlockMark >> cLineSpace >> void "scriba"
  cSpace
  atIndent (posIndent sp + 1) $ do
    atDeindent (Attrs [] []) $ do
      ia <- pInlineAttrs cSpace
      atDeindent (Attrs ia []) $ do
        ba <- pBlockAttrs cSpace
        pure $ Attrs ia ba

pSecContent :: Parser [SecNode]
pSecContent = MP.many $ pSecNode <* cSpace

-- | A section node is either a block node or a section header

-- TODO: This assumes that section nodes occur at zero indent!
pSecNode :: Parser SecNode
pSecNode = SecHeaderNode <$> pSecHeader <|> SecBlock <$> pSecBlock

-- | A section header is a sequence of one or more number signs, then
-- an element type and attributes.

-- TODO: duplication with BlockElement
-- TODO: not sure how I like the pNilAttributes handling.
pSecHeader :: Parser SecHeader
pSecHeader = do
  sp <- MP.getSourcePos
  n  <- pNumberRun
  cLineSpace
  mty <- MP.optional pElemTy
  cLineSpace
  at <- pSecAttributes
  pure $ SecHeader n sp mty at []
 where
  pNumberRun         = T.length <$> MP.takeWhile1P Nothing (== '#')
  pSecAttributes     = pNilAttributes <|> pPresentAttributes
  pNilAttributes     = MP.try $ pBlankLine $> Attrs [] []
  pPresentAttributes = atIndent 1 $ do
    cSpace
    atDeindent (Attrs [] []) $ do
      ia <- pInlineAttrs cSpace
      atDeindent (Attrs ia []) $ do
        ba <- pBlockAttrs cSpace
        pure $ Attrs ia ba

-- TODO: observe that this assumes an ambient indent of 0.
pSecBlock :: Parser BlockNode
pSecBlock = do
  sp <- MP.getSourcePos
  BlockBlock
    <$> pBlockElement (MP.optional pElemTy)
    <|> BlockPar sp
    <$> pParagraph

-- | Parse a block element. The passed @SourcePos@ will set the
-- indentation level and position of the parsed 'Element'.
pBlockElement :: Parser ty -> Parser (Element ty BlockContent)
pBlockElement pTy = do
  sp <- MP.getSourcePos
  pBlockMark
  cLineSpace
  atIndent (posIndent sp + 1) $ do
    ty <- pTy
    cSpace
    ia <- pInlineAttrs cSpace
    atDeindent (Element sp ty (Attrs ia []) [] BlockNil) $ do
      ba <- pBlockAttrs cSpace
      atDeindent (Element sp ty (Attrs ia ba) [] BlockNil) $ do
        as <- MP.option [] $ pArgs cSpace
        atDeindent (Element sp ty (Attrs ia ba) as BlockNil) $ do
          c <- pBlockContent
          pure $ Element sp ty (Attrs ia ba) as c

manyIndented :: Parser space -> Parser a -> Parser [a]
manyIndented sc p = go id
 where
  go f = atDeindent (f []) $ do
    ma <- MP.optional p
    case ma of
      Nothing -> pure $ f []
      Just a  -> sc >> go (f . (a :))

-- | Parse a sequence of zero or more properly-indented block
-- attributes.
pBlockAttrs :: Parser space -> Parser [BlockAttr]
pBlockAttrs = flip manyIndented $ pBlockElement pElemTy

-- Not sure if a separate version of this that forbids blank lines
-- _inside_ the inline element is worth it.
pInlineAttrs :: Parser space -> Parser [InlineAttr]
pInlineAttrs = flip manyIndented $ pInlineElement pElemTy

pArgs :: Parser space -> Parser [InlineNode]
pArgs sc = pElementArgsStart >> sc >> MP.many (pArgNode <* sc)
  where pArgNode = pInlineNodeWith pArgText

-- TODO: a version of this for section attributes that forbids blank
-- lines?
pInlineElement :: Parser ty -> Parser (Element ty InlineContent)
pInlineElement pTy = pBraced "inline element" $ \src -> do
  t   <- (cSpace >> guardIndented >> pTy) <* cSpace
  at  <- guardIndented >> pInlineAttrs cSpace
  ar  <- guardIndented >> MP.option [] (pArgs cSpace)
  con <- guardIndented >> pInlineContent
  pure $ Element src t (Attrs at []) ar con

-- TODO: should the block content markers start their own indentation context?
pBlockContent :: Parser BlockContent
pBlockContent =
  pBlockParContent <|> pBlockUnparContent <|> pBlockVerbContent <|> pBlockNil

pBlockParContent :: Parser BlockContent
pBlockParContent = do
  void pBlockBodyStart
  cSpace >> guardIndented
  BlockBlocks <$> pBlockNodes

-- TODO: can I just use the inline seq content parser from inline element?
pBlockUnparContent :: Parser BlockContent
pBlockUnparContent = BlockInlines <$> pSeq
  where pSeq = pInlineBodyStart >> MP.many (pInlineNodeWith pInlineText)

pBlockNil :: Parser BlockContent
pBlockNil = MP.label "de-indented content" $ do
  sp <- MP.getSourcePos
  let n = posIndent sp
  lvl <- getIndent
  if n < lvl then pure BlockNil else empty

pBlockVerbContent :: Parser BlockContent
pBlockVerbContent = do
  src <- MP.getSourcePos
  void pVerbatimBlockBodyStart
  cLineSpace
  t <- pBlockVerbText
  pure $ BlockVerbatim src $ stripNewline t
  where stripNewline t = maybe t snd $ T.uncons t

pBlockVerbText :: Parser Text
pBlockVerbText = fmap T.concat $ MP.many blankThenText
 where
  blankThenText = MP.try $ do
    ts   <- MP.many pBlankLine
    t    <- indentText
    rest <- MP.takeWhileP Nothing (/= '\n')
    pure $ T.concat $ ts <> [t, rest]

pBlockNodes :: Parser [BlockNode]
pBlockNodes = manyIndented cSpace pBlockNode

pBlockNode :: Parser BlockNode
pBlockNode = BlockBlock <$> pBlockElement (MP.optional pElemTy) <|> pPar
 where
  pPar = do
    sp <- MP.getSourcePos
    p  <- pParagraph
    pure $ BlockPar sp p

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

pParagraph :: Parser [InlineNode]
pParagraph = MP.some pParaNode where pParaNode = pInlineNodeWith pParaText

pInlineNodeWith :: Parser Text -> Parser InlineNode
pInlineNodeWith pText = do
  sp  <- MP.getSourcePos
  eet <- MP.eitherP (pCommentStart >> pCommentBody)
    $ MP.eitherP (pInlineElement (MP.optional pElemTy)) pText
  pure $ case eet of
    Left  c         -> InlineComment sp c
    Right (Left  e) -> InlineBraced e
    Right (Right t) -> InlineText sp t

-- ** Comments and whitespace

pLineSpace1 :: Parser Text
pLineSpace1 = MP.takeWhile1P (Just "line space") $ \c -> isSpace c && c /= '\n'

pLineSpace :: Parser Text
pLineSpace = MP.takeWhileP (Just "line space") $ \c -> isSpace c && c /= '\n'

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

-- | Parses a literal @```@, the start of a verbatim block body.
pVerbatimBlockBodyStart :: Parser Text
pVerbatimBlockBodyStart = "```" <?> "start of verbatim body"

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
-- TODO: rethink comments in the presence of indentation
-- sensitivity. If they're just a form of ignored inline node then
-- they should be parsed as such.
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
-- underscores, and periods. This may be changed in future.
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

-- | Parse a newline, as long as the subsequent line is properly
-- indented and not blank.
pParaIndent :: Parser Text
pParaIndent = MP.try $ do
  void "\n"
  cLineSpace
  MP.notFollowedBy (void "\n" <|> MP.eof)
  guardIndented
  pure "\n"

-- | Parse indented paragraph text.
pParaText :: Parser Text
pParaText =
  MP.label "paragraph text"
    $   fmap T.concat
    $   MP.some
    $   pInlineTextLine
    <|> pLineSpace1
    <|> pParaIndent

-- TODO: try?
indentText :: Parser Text
indentText = MP.try $ do
  void "\n"
  t <- pLineSpace
  guardIndented
  lvl <- getIndent
  t'  <- splitIndent lvl t
  pure $ "\n" <> t'

-- | Parse indented text that may end with a de-indent or end of
-- input.
pInlineText :: Parser Text
pInlineText =
  MP.label "inline text"
    $   fmap T.concat
    $   MP.some
    $   pInlineTextLine
    <|> subsequentlyIndented
 where
  subsequentlyIndented = MP.try $ do
    void "\n"
    t <- MP.takeWhileP Nothing isSpace
    MP.notFollowedBy "\n"
    guardIndented
    pure $ "\n" <> T.filter (== '\n') t

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

pInlineVerbText :: Parser Text
pInlineVerbText =
  fmap T.concat
    $   MP.many
    $   pInsig
    <|> pBacktickEsc
    <|> pInsigBacktick
    <|> pBlankLine
    <|> indentText
 where
  pInsig =
    MP.takeWhile1P Nothing (\c -> c /= '`' && c /= '\n')
      <?> "non-backtick characters"
  pBacktickEsc = MP.chunk "``" $> "`"
  pInsigBacktick =
    MP.label "'`' not before '}'" $ MP.try $ MP.chunk "`" <* MP.notFollowedBy
      (MP.single '}')

-- | Parses something between the start and end inline element
-- braces. Also takes a description of what the thing is, for
-- labelling the braces.
pBraced :: String -> (SourcePos -> Parser a) -> Parser a
pBraced t p = do
  src <- MP.getSourcePos
  MP.between ("{" <?> "start of " <> t) ("}" <?> "end of " <> t) $ p src

-- * Parsing

parseWithAt :: Parser a -> Text -> Int -> Text -> Either ParseError a
parseWithAt p fn n = MP.parse (runParser p n) (T.unpack fn)

parseWithAt' :: Parser a -> Text -> Int -> Text -> Either Text a
parseWithAt' p fn n =
  either (Left . T.pack . MP.errorBundlePretty) Right . parseWithAt p fn n

parseWith :: Parser a -> Text -> Text -> Either ParseError a
parseWith p fn = parseWithAt p fn 0

parseWith' :: Parser a -> Text -> Text -> Either Text a
parseWith' p fn =
  either (Left . T.pack . MP.errorBundlePretty) Right . parseWith p fn

parseDoc' :: Text -> Text -> Either Text Doc
parseDoc' = parseWith' pDoc

parseTesting :: Parser a -> Int -> Text -> Either Text (Text, a)
parseTesting p inp = parseWithAt' go "<test>" inp
 where
  go = do
    a <- p
    s <- MP.getParserState
    pure (MP.stateInput s, a)
