{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Text.Scriba.Source.Parse
Description : Text stream parsers
Copyright   : 2020 Christian Despres
License     : BSD-2-Clause
Maintainer  : Christian Despres
Stability   : experimental

This module defines text parsers for scriba documents, returning
"Text.Scriba.Source.Common" types. These are intended to be passed off
to "Text.Scriba.Intermediate.Node" for further processing.
-}


module Text.Scriba.Source.Parse
  ( -- * The Parser monad

    -- $parserdesc
    Parser(..)
  , ParseError
  , runParser

  -- * Indentation and white space
  , getIndent
  , atIndent
  , dropIndent
  , posIndent
  , cSpace
  , cLineSpace
  , pLineSpace
  , pLineSpace1
  , pBlankLine
  , ifIndented
  , atDeIndent
  , guardIndented
  , manyIndented

  -- * Components of elements
  , pElemTy
  , pInlineBodyStart
  , pBlockBodyStart
  , pVerbatimBodyStart
  , pVerbatimBlockBodyStart
  , pVerbatimBodyEnd
  , pCommentStart
  , pCommentEnd
  , pBlockMark
  , pElementArgsStart
  , pBraced

  -- * Text and comments
  , pCommentBody
  , pBackslashToks
  , pInlineText
  , indentText
  , pParaIndent
  , pParaText
  , pParaWhite
  , pInlineWhite
  , pArgText
  , pInlineVerbText
  , pBlockVerbText

  -- * Document elements and nodes
  , pDoc
  , pDocAttrs
  , pSecContent
  , pSecNode
  , pSecHeader
  , pSecBlock
  , pBlockElement
  , pBlockAttrs
  , pInlineAttrs
  , pArgs
  , pInlineElement
  , pBlockContent
  , pBlockParContent
  , pBlockUnparContent
  , pBlockVerbContent
  , pBlockNil
  , pBlockNode
  , pBlockNodes
  , pInlineContent
  , pParagraph
  , pInlineNodeWith

  -- * Running parsers
  , parseWithAt
  , parseWithAt'
  , parseWith
  , parseWith'
  , parseDoc'
  , parseTesting
  )
where

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

- Parse regions. I want "when parsing ..." errors.

- Change the indentation definitions around? Might be simpler to have
  the ambient indent represent the column _beyond_ which the elements
  should lie, so that a de-indent occurs if the source level is less
  than or equal to the ambient level.

- Consider getting rid of the p* naming convention.

- In the documentation, clarify that "properly indentated" means "must
  occur at or beyond a particular source column"
-}

-- $parserdesc
--
-- Scriba is an indentation-sensitive language; the content of block
-- elements ends when a line is encountered that is not indented by
-- more than the block marker, and the indentation (and all leading
-- line space, in some cases) is stripped from the content. Thus the
-- parsers for every syntactic form that can occur inside a block
-- (inline and block things and text) must be aware of source
-- indentation (the indentation at a particular stream position) and
-- the current ambient indentation level.
--
-- These parsers use a simple indentation strategy. Line space
-- non-white-space text may be parsed normally. Newlines are only
-- successfully parsed if they are followed by a properly-indented
-- line. The main element parsers assume that all whitespace before
-- them has been dealt with and the indentation level has been
-- checked. Indentation itself _must_ be in the form of white space
-- characters; comments are parsed as inline nodes and so can only
-- appear where inline nodes can. It is also recommended that these
-- characters be the @' '@ space character, since tabs are not yet
-- supported as indentation for verbatim blocks (see 'dropIndent').
--
-- In the documentation that follows, the source indentation level is
-- the current column position of the stream, and something is
-- considered properly indented if its source indentation (converted
-- to an @Int@) is greater than the ambient indentation level. The
-- ambient indentation at the start of a document is generally going
-- to be @0@.

-- | A 'Parser' is a megaparsec parser that is to be run with an
-- initial ambient indentation level equal to the @Int@ in the
-- @ReaderT@.
newtype Parser a = Parser
  { getParser :: ReaderT Int (Parsec Void Text) a
  } deriving ( Functor
             , Applicative
             , Monad
             , Alternative
             , MonadPlus
             , MP.MonadParsec Void Text
             , MonadFail )

instance (a ~ Text) => IsString (Parser a) where
  fromString = Parser . lift . fromString

instance Semigroup a => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

-- | Unwrap the given 'Parser'.
runParser :: Parser a -> Int -> Parsec Void Text a
runParser = runReaderT . getParser

-- | The type of a 'Parser' error, which contains no custom error
-- components.
type ParseError = ParseErrorBundle Text Void

-- | Retrieve the ambient indentation level.
getIndent :: Parser Int
getIndent = Parser Reader.ask

-- | Run the given parser at the given indentation level.
atIndent :: Int -> Parser a -> Parser a
atIndent n = Parser . Reader.local (const n) . getParser

-- | In @dropIndent n txt@, drop the first @n@ spaces from
-- @txt@ and return the remainder. This _ought_ to be aware of tab
-- indentation as well, but is not yet.
dropIndent :: Int -> Text -> Parser Text
dropIndent n = pure . T.drop n

-- | Extract the indentation level from a 'SourcePos'.
posIndent :: SourcePos -> Int
posIndent = MP.unPos . MP.sourceColumn

-- | Consumes any white space character.
cSpace :: Parser ()
cSpace = void $ MP.takeWhileP Nothing isSpace

-- | Consumes any white space character other than a newline.
cLineSpace :: Parser ()
cLineSpace = void $ pLineSpace

-- | Parse one or more line space characters.
pLineSpace1 :: Parser Text
pLineSpace1 = MP.takeWhile1P (Just "line space") $ \c -> isSpace c && c /= '\n'

-- | Parse zero or more line space characters.
pLineSpace :: Parser Text
pLineSpace = MP.takeWhileP (Just "line space") $ \c -> isSpace c && c /= '\n'

-- | Parse a blank line, in this case meaning a single newline and
-- subsequent line space, followed by a newline. The terminal newline
-- is not consumed.

-- TODO: try?
pBlankLine :: Parser Text
pBlankLine = MP.label "blank line" $ MP.try $ do
  void "\n"
  t <- pLineSpace
  void $ MP.lookAhead "\n"
  pure $ "\n" <> t

-- | The @'ifIndented' low high@ parser runs @high@ if the stream is
-- properly indented, and otherwise runs @low@ after passing it the
-- source indentation and the ambient indentation.
ifIndented :: (Int -> Int -> Parser a) -> Parser a -> Parser a
ifIndented pLow pHigh = do
  sp <- MP.getSourcePos
  let n = posIndent sp
  lvl <- getIndent
  if n > lvl then pHigh else pLow n lvl

-- | Run the supplied parser if the stream is properly indented, and
-- otherwise return the supplied value.
atDeIndent :: a -> Parser a -> Parser a
atDeIndent low = ifIndented (const $ const $ pure low)

-- | Fail if the stream is not properly indented.
guardIndented :: Parser ()
guardIndented =
  ifIndented
      (\n lvl ->
        fail
          $  "insufficient indent: got "
          <> show n
          <> ", expected "
          <> show lvl
      )
    $ pure ()

-- | The parser @'manyIndented' sc p@ parses zero or more
-- properly-indented occurrences of @p@ separated by @sc@.
manyIndented :: Parser space -> Parser a -> Parser [a]
manyIndented sc p = go id
 where
  go f = atDeIndent (f []) $ do
    ma <- MP.optional p
    case ma of
      Nothing -> pure $ f []
      Just a  -> sc >> go (f . (a :))

-- | An element type is a sequence of alphanumeric characters,
-- underscores, and periods.
pElemTy :: Parser Text
pElemTy = MP.takeWhile1P Nothing isTyChar <?> "element type"
  where isTyChar x = isAlphaNum x || x == '_' || x == '.'

-- | Parses a literal @|@, the start of an inline body for block and
-- inline elements.
pInlineBodyStart :: Parser Text
pInlineBodyStart = "|" <?> "start of inline body"

-- | Parses a literal @&@, the start of a paragraphed body for block
-- elements.
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

-- | Parses a literal @{%@, the start of a comment.
pCommentStart :: Parser Text
pCommentStart = "{%" <?> "start of comment"

-- | Parses a literal @%}@, the end of a comment.
pCommentEnd :: Parser Text
pCommentEnd = "%}" <?> "end of comment"

-- | Parses a literal @&@, the start of a block element.
pBlockMark :: Parser ()
pBlockMark = void "&" <?> "block marker"

-- | Parses a literal @\@@, the start of the arguments of an
-- element.
pElementArgsStart :: Parser Text
pElementArgsStart = "@" <?> "start of arguments"

-- | Parses something between opening and closing braces. Also takes a
-- name of what the thing is, for labelling the braces.
pBraced :: String -> (SourcePos -> Parser a) -> Parser a
pBraced t p = do
  src <- MP.getSourcePos
  MP.between ("{" <?> "start of " <> t) ("}" <?> "end of " <> t) $ p src

-- | Parses the body of a nested comment.

-- TODO: should probably accumulate with a snoc list, or a text
-- builder. That might be true of the plain text parsers as well,
-- honestly.
-- TODO: rethink comments in the presence of indentation
-- sensitivity. If they're just a form of ignored inline node then
-- they should be parsed as such.
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


-- | Parses the recognized backslash tokens in plain text. These are
-- @\\@ followed by one of @{}\\@, standing for the second character,
-- and @\\@ not before one of these, standing for @\\@.
pBackslashToks :: Parser Text
pBackslashToks = MP.single '\\' >> MP.option "\\" pEscChar
  where pEscChar = fmap T.singleton $ MP.satisfy $ \c -> T.any (== c) "{}\\"

-- | Parse line text, a sequence of characters other than @{}\\@ or
-- spaces.
pInlineText :: Parser InlineNode
pInlineText = MP.label "inline text" $ do
  sp <- MP.getSourcePos
  ts <- MP.some $ insigChar <|> pBackslashToks
  pure $ InlineText sp (T.concat ts)
 where
  insigChar =
    MP.takeWhile1P Nothing $ \c -> not $ T.any (== c) "\\{}" || isSpace c

-- | Parse a newline character, as long as there is properly-indented
-- text on the subsequent line. Returns the newline and any excess
-- indent.
indentText :: Parser Text
indentText = do
  void "\n"
  t <- pLineSpace
  guardIndented
  lvl <- getIndent
  t'  <- dropIndent lvl t
  pure $ "\n" <> t'

-- | Parse a newline, as long as the subsequent line is properly
-- indented and not blank.
pParaIndent :: Parser InlineNode
pParaIndent = MP.try $ do
  src <- MP.getSourcePos
  void "\n"
  cLineSpace
  MP.notFollowedBy (void "\n" <|> MP.eof)
  guardIndented
  pure $ InlineWhite src "\n"

-- | Parse indented paragraph text.
pParaText :: Parser InlineNode
pParaText = MP.label "paragraph text" $ pInlineText <|> pParaWhite

-- | Parse paragraph white space.

-- TODO: have a top-level pLineSpace1 that returns an InlineNode?
-- Otherwise perhaps merge some of these parsers together.
pParaWhite :: Parser InlineNode
pParaWhite = MP.label "white space" $ pParaIndent <|> pLS
 where
  pLS = do
    sp <- MP.getSourcePos
    t  <- pLineSpace1
    pure $ InlineWhite sp t

-- | Parse line space, then an optional sequence of blank lines
-- followed by properly-indented content.

-- TODO: the MP.some implementation is not great. Should instead parse
-- one span of line space, then one span of blank lines, ensuring that
-- the parser still fails on empty input.
pInlineWhite :: Parser InlineNode
pInlineWhite = MP.label "white space" $ do
  src <- MP.getSourcePos
  ts  <- MP.some $ pLineSpace1 <|> subsequentlyIndented
  pure $ InlineWhite src $ T.concat ts
 where
  subsequentlyIndented = MP.try $ do
    void "\n"
    t <- MP.takeWhileP Nothing isSpace
    guardIndented
    pure $ "\n" <> T.filter (== '\n') t

-- | Argument text is inline text except that it cannot contain spaces
-- or a character from @|&`@.
pArgText :: Parser InlineNode
pArgText = MP.label "argument text" $ do
  sp <- MP.getSourcePos
  ts <- MP.some $ insigChar <|> pBackslashToks
  pure $ InlineWhite sp (T.concat ts)
 where
  insigChar =
    MP.takeWhile1P Nothing $ \c -> not $ T.any (== c) "\\{}|`&" || isSpace c

-- | Parse a sequence of inline verbatim text. The leading space on a
-- line should be @' '@ only, otherwise indent stripping will lead to
-- errors. See 'indentText' and 'dropIndent'.
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

-- | Parse block verbatim text. Note that leading space on a line
-- should be @' '@ only, otherwise indent stripping will lead to
-- errors. See 'indentText' and 'dropIndent'.
pBlockVerbText :: Parser Text
pBlockVerbText = T.concat <$> MP.many blankThenText
 where
  blankThenText = MP.try $ do
    ts   <- MP.many pBlankLine
    t    <- indentText
    rest <- MP.takeWhileP Nothing (/= '\n')
    pure $ T.concat $ ts <> [t, rest]

-- | Parse a scriba document
pDoc :: Parser Doc
pDoc = do
  src <- MP.getSourcePos
  cSpace
  at <- MP.option (Attrs [] []) pDocAttrs
  cSpace
  c <- pSecContent
  MP.eof
  pure $ Doc src at c

-- | Parse the attributes of a scriba document.

-- TODO: duplication with BlockElement
pDocAttrs :: Parser Attrs
pDocAttrs = do
  sp <- MP.getSourcePos
  MP.try $ pBlockMark >> cLineSpace >> void "scriba"
  cSpace
  atIndent (posIndent sp) $ atDeIndent (Attrs [] []) $ do
    ia <- pInlineAttrs cSpace
    atDeIndent (Attrs ia []) $ do
      ba <- pBlockAttrs cSpace
      pure $ Attrs ia ba

-- | Parse section content
pSecContent :: Parser [SecNode]
pSecContent = MP.many $ pSecNode <* cSpace

-- | Parse a section node

-- TODO: This assumes that section nodes occur at zero indent!
pSecNode :: Parser SecNode
pSecNode = SecHeaderNode <$> pSecHeader <|> SecBlock <$> pSecBlock

-- | Parse a section header

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
    atDeIndent (Attrs [] []) $ do
      ia <- pInlineAttrs cSpace
      atDeIndent (Attrs ia []) $ do
        ba <- pBlockAttrs cSpace
        pure $ Attrs ia ba

-- | Parse a block occurring as a section node.

-- TODO: observe that this assumes an ambient indent of 0.
pSecBlock :: Parser BlockNode
pSecBlock = do
  sp <- MP.getSourcePos
  BlockBlock
    <$> pBlockElement (MP.optional pElemTy)
    <|> BlockPar sp
    <$> pParagraph

-- | Parse a block element.
pBlockElement :: Parser ty -> Parser (Element ty BlockContent)
pBlockElement pTy = do
  sp <- MP.getSourcePos
  pBlockMark
  cLineSpace
  atIndent (posIndent sp) $ do
    ty <- pTy
    cSpace
    ia <- pInlineAttrs cSpace
    atDeIndent (Element sp ty (Attrs ia []) [] BlockNil) $ do
      ba <- pBlockAttrs cSpace
      atDeIndent (Element sp ty (Attrs ia ba) [] BlockNil) $ do
        as <- MP.option [] $ pArgs cSpace
        atDeIndent (Element sp ty (Attrs ia ba) as BlockNil)
          $   Element sp ty (Attrs ia ba) as
          <$> pBlockContent

-- | Parse a sequence of zero or more properly-indented block
-- attributes, separated by @space@.
pBlockAttrs :: Parser space -> Parser [BlockAttr]
pBlockAttrs = flip manyIndented $ pBlockElement pElemTy

-- | Parse a sequence of zero or more properly-indented inline
-- attributes.

-- Not sure if a separate version of this that forbids blank lines
-- _inside_ the inline element is worth it.
pInlineAttrs :: Parser space -> Parser [InlineAttr]
pInlineAttrs = flip manyIndented $ pInlineElement pElemTy

-- | Parse the arguments of an element.
pArgs :: Parser space -> Parser [InlineNode]
pArgs sc = pElementArgsStart >> sc >> MP.many (pArgNode <* sc)
  where pArgNode = pInlineNodeWith pArgText

-- | Parse an inline element.

-- TODO: a version of this for section attributes that forbids blank
-- lines?
pInlineElement :: Parser ty -> Parser (Element ty InlineContent)
pInlineElement pTy = pBraced "inline element" $ \src -> do
  t   <- (cSpace >> guardIndented >> pTy) <* cSpace
  at  <- guardIndented >> pInlineAttrs cSpace
  ar  <- guardIndented >> MP.option [] (pArgs cSpace)
  con <- guardIndented >> pInlineContent
  pure $ Element src t (Attrs at []) ar con

-- | Parse one of the recognized forms of block content.
pBlockContent :: Parser BlockContent
pBlockContent =
  pBlockParContent <|> pBlockUnparContent <|> pBlockVerbContent <|> pBlockNil

-- | Parse paragraphed block content.
pBlockParContent :: Parser BlockContent
pBlockParContent = do
  void pBlockBodyStart
  cSpace >> guardIndented
  BlockBlocks <$> pBlockNodes

-- | Parse unparagraphed block content.

-- TODO: can I just use the inline seq content parser from inline element?
pBlockUnparContent :: Parser BlockContent
pBlockUnparContent = BlockInlines <$> pSeq
 where
  pSeq =
    pInlineBodyStart >> MP.many (pInlineNodeWith (pInlineText <|> pInlineWhite))

-- | Parse block verbatim content.
pBlockVerbContent :: Parser BlockContent
pBlockVerbContent = do
  src <- MP.getSourcePos
  void pVerbatimBlockBodyStart
  cLineSpace
  BlockVerbatim src . stripNewline <$> pBlockVerbText
  where stripNewline t = maybe t snd $ T.uncons t

-- | Parse empty block content.
pBlockNil :: Parser BlockContent
pBlockNil = MP.label "de-indented content" $ atDeIndent BlockNil empty

-- | Parse a single paragraphed block node.
pBlockNode :: Parser BlockNode
pBlockNode = BlockBlock <$> pBlockElement (MP.optional pElemTy) <|> pPar
 where
  pPar = do
    sp <- MP.getSourcePos
    BlockPar sp <$> pParagraph

-- | Parse a sequence of paragraphed block nodes.
pBlockNodes :: Parser [BlockNode]
pBlockNodes = manyIndented cSpace pBlockNode

-- | Parse the content of an inline element.
pInlineContent :: Parser InlineContent
pInlineContent = MP.option InlineNil $ pInlineSeqBody <|> pInlineVerbBody
 where
  pInlineSeqBody = fmap InlineSequence $ pInlineBodyStart >> MP.many
    (pInlineNodeWith (pInlineText <|> pInlineWhite))
  pInlineVerbBody = do
    src <- MP.getSourcePos
    -- TODO: better label?
    InlineVerbatim src
      <$> MP.between pVerbatimBodyStart pVerbatimBodyEnd pInlineVerbText

-- | Parse a paragraph.
pParagraph :: Parser [InlineNode]
pParagraph = MP.some pParaNode where pParaNode = pInlineNodeWith pParaText

-- | Parse an inline node with the given text parser.
pInlineNodeWith :: Parser InlineNode -> Parser InlineNode
pInlineNodeWith pText = do
  sp  <- MP.getSourcePos
  eet <- MP.eitherP (pCommentStart >> pCommentBody)
    $ MP.eitherP (pInlineElement (MP.optional pElemTy)) pText
  pure $ case eet of
    Left  c         -> InlineComment sp c
    Right (Left  e) -> InlineBraced e
    Right (Right e) -> e

-- | Run the given parser with the given input name and ambient
-- indentation level.
parseWithAt :: Parser a -> Text -> Int -> Text -> Either ParseError a
parseWithAt p fn n = MP.parse (runParser p n) (T.unpack fn)

-- | Run the given parser with the given input name and ambient
-- indentation level, and also render any parse error that is thrown.
parseWithAt' :: Parser a -> Text -> Int -> Text -> Either Text a
parseWithAt' p fn n =
  either (Left . T.pack . MP.errorBundlePretty) Right . parseWithAt p fn n

-- | Run the given parser with the given input name and text input at
-- an ambient indentation level of @0@.
parseWith :: Parser a -> Text -> Text -> Either ParseError a
parseWith p fn = parseWithAt p fn 0

-- | Run the given parser with the given input name and text input at
-- an ambient indentation level of @0@, and also render any parse
-- error that is thrown.
parseWith' :: Parser a -> Text -> Text -> Either Text a
parseWith' p fn =
  either (Left . T.pack . MP.errorBundlePretty) Right . parseWith p fn

-- | Parse an entire scriba document, returning either a rendered
-- parse error or the resulting document.
parseDoc' :: Text -> Text -> Either Text Doc
parseDoc' = parseWith' pDoc

-- | Run a given parser at the given indentation level, returning
-- either a rendered parse error, or the remaining input and the
-- returned value.
parseTesting :: Parser a -> Int -> Text -> Either Text (Text, a)
parseTesting p = parseWithAt' go "<test>"
 where
  go = do
    a <- p
    s <- MP.getParserState
    pure (MP.stateInput s, a)
