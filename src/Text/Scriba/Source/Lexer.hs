{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Scriba.Source.Lexer where

{-
import           Control.Applicative            ( (<|>)
                                                , empty
                                                , Alternative
                                                , liftA2
                                                )
import           Control.Monad                  ( MonadPlus )
import           Control.Monad.Reader           ( Reader
                                                , runReaderT
                                                , lift
                                                , ask
                                                )
import qualified Control.Monad.Reader          as Reader
import           Data.Bifunctor                 ( bimap )
import           Data.Char                      ( isAlphaNum
                                                , isSpace
                                                )
import           Data.Foldable                  ( toList )
import           Data.Functor                   ( void
                                                , ($>)
                                                )
import qualified Data.List                     as List
import qualified Data.List.NonEmpty            as NE
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.Proxy                     ( Proxy(..) )
import           Data.String                    ( IsString(..) )
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
-}

{-
data Located = Located !SourcePos !SourcePos !Tok
  deriving (Eq, Ord, Show)

locatedTok :: Located -> Tok
locatedTok (Located _ _ t) = t

locatedStart :: Located -> SourcePos
locatedStart (Located s _ _) = s

locatedEnd :: Located -> SourcePos
locatedEnd (Located _ e _) = e

data Tok
  = Text !Text
  | Indent !Int !Text
  | LineWhite !Text
  | At
  | StartComment
  | EndComment
  | Lbrace
  | Rbrace
  | Pipe
  | Amp
  | Dashes
  | Dots
  | Backtick
  | Backslash
  | Nums !Int
  deriving (Eq, Ord, Show)

fromTok :: Tok -> Text
fromTok (Text t     ) = t
fromTok (Indent _ t ) = "\n" <> t
fromTok (LineWhite t) = t
fromTok At            = "@"
fromTok StartComment  = "{%"
fromTok EndComment    = "%}"
fromTok Lbrace        = "{"
fromTok Rbrace        = "}"
fromTok Pipe          = "|"
fromTok Amp           = "&"
fromTok Dashes        = "---"
fromTok Dots          = "..."
fromTok Backtick      = "`"
fromTok Backslash     = "\\"
fromTok (Nums n)      = T.replicate n "#"

type Parser = Parsec Void Text

type ParseError = ParseErrorBundle Text Void

insigChar :: Char -> Bool
insigChar c = not $ T.any (== c) cs || isSpace c where cs = "@{}|&.-`\\%#"

pText :: Parser Tok
pText = fmap (Text . T.concat) $ MP.some $ pPlain <|> pInsig
 where
  pPlain        = MP.takeWhile1P Nothing insigChar
  pInsigDot     = "." <* MP.notFollowedBy ".."
  pInsigDash    = "-" <* MP.notFollowedBy "--"
  pInsigPercent = "%" <* MP.notFollowedBy "}"
  pInsig        = pInsigDot <|> pInsigDash <|> pInsigPercent

pIndent :: Parser Tok
pIndent = do
  void "\n"
  t <- MP.takeWhileP Nothing (' ' ==)
  pure $ Indent (T.length t) t

pLineWhite :: Parser Tok
pLineWhite = do
  t <- MP.takeWhile1P (Just "line space") $ \c -> isSpace c && c /= '\n'
  pure $ LineWhite t

pAt :: Parser Tok
pAt = "@" $> At

pStartComment :: Parser Tok
pStartComment = "{%" $> StartComment

pEndComment :: Parser Tok
pEndComment = "%}" $> EndComment

pLbrace :: Parser Tok
pLbrace = "{" $> Lbrace

pRbrace :: Parser Tok
pRbrace = "}" $> Rbrace

pPipe :: Parser Tok
pPipe = "|" $> Pipe

pAmp :: Parser Tok
pAmp = "&" $> Amp

pDashes :: Parser Tok
pDashes = "---" $> Dashes

pDots :: Parser Tok
pDots = "..." $> Dots

pBacktick :: Parser Tok
pBacktick = "`" $> Backtick

pBackslash :: Parser Tok
pBackslash = "\\" $> Backslash

pNums :: Parser Tok
pNums = Nums . T.length <$> MP.takeWhile1P Nothing ('#' ==)

pTok :: Parser Tok
pTok =
  pText
    <|> pIndent
    <|> pLineWhite
    <|> pAt
    <|> pStartComment
    <|> pEndComment
    <|> pLbrace
    <|> pRbrace
    <|> pPipe
    <|> pAmp
    <|> pDashes
    <|> pDots
    <|> pBacktick
    <|> pBackslash
    <|> pNums

pToks :: Parser ([(SourcePos, Tok)], SourcePos)
pToks = do
  ts <- MP.many $ liftA2 (,) MP.getSourcePos pTok
  MP.eof
  p <- MP.getSourcePos
  pure (ts, p)
pLocatedToks :: Parser [Located]
pLocatedToks = do
  (ts, spEnd) <- pToks
  pure $ go spEnd ts
 where
  go spEnd ((sp, t) : y@((sp', _) : _)) = Located sp sp' t : go spEnd y
  go spEnd [(sp, t)                   ] = [Located sp spEnd t]
  go _     []                           = []

newtype TokStream = TokStream
  { getTokStream :: [Located]
  } deriving (Eq, Ord, Semigroup, Monoid)

consStream :: Located -> TokStream -> TokStream
consStream l (TokStream ls) = TokStream (l : ls)

splitStream :: Int -> TokStream -> (TokStream, TokStream)
splitStream n = bimap TokStream TokStream . splitAt n . getTokStream

splitStreamPos :: Int -> SourcePos -> TokStream -> (TokStream, SourcePos, TokStream)
splitStreamPos m sp (TokStream (l : ls))
  | m <= 0
  = (TokStream [], locatedStart l, TokStream (l : ls))
  | otherwise
  = let (pre, sp', post) = splitStreamPos (m - 1) sp (TokStream ls)
    in  (consStream l pre, sp', post)
splitStreamPos _ sp (TokStream []) = (TokStream [], sp, TokStream [])

nullStream :: TokStream -> Bool
nullStream (TokStream []) = True
nullStream _              = False

instance MP.Stream TokStream where
  type Token TokStream = Located
  type Tokens TokStream = TokStream
  tokensToChunk _ = TokStream
  chunkToTokens _ = getTokStream
  chunkLength _ = length . getTokStream
  take1_ = fmap (fmap TokStream) . List.uncons . getTokStream
  takeN_ n s | n <= 0       = Nothing
             | nullStream s = Nothing
             | otherwise    = Just $ splitStream n s
  takeWhile_ p = bimap TokStream TokStream . span p . getTokStream
  showTokens _ = go . T.unpack . T.concat . map (fromTok . locatedTok) . toList
   where
    go s = case NE.nonEmpty s of
      Nothing -> "<empty>"
      Just ne -> MP.showTokens (Proxy :: Proxy String) ne
  reachOffset o (MP.PosState inp o' sp tw lp) = (pref, MP.PosState post (max o o') sp' tw lp')
   where
     (pre, sp', post) = splitStreamPos (o - o') sp inp
     updatePref f tok = case locatedTok tok of
       x@Indent{} -> undefined
-}
