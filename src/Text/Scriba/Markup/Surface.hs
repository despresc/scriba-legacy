{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Text.Scriba.Markup.Surface where

import           Text.Scriba.Counters

import           Data.Map.Strict                ( Map )
import           Data.Set                       ( Set )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

{- TODO:

- Consider recursion schemes, e.g. parametrizing on Block and Inline
  inside Block and Inline. Might be helpful.

- Consider parametrizing various types on the compiler stage that is
  being run. E.g. could have NoteText have no num attribute at all at
  the beginning, then get one after the Numbering stage.

- How should regularize interact with numbering? Might want to disable
  automatic numbering within it. Otherwise we need to have branching
  numbering states.

- More interesting DisplayMath environments. Also more consistent form
  (it's not a wrapper with a univAttrs selector, for instance), and
  greater flexibility (being able to Gather arbitrary pieces of math)
  and configurability (being able to Gather a group of equations and
  number the entire thing, a la Cauchy).

- Hook PageMark up to the linking system, and be able to link to
  different digitized sources.

- NoteMark might want to be integrated into the reference system as
  well. Would simply be an alias for a particular type of reference to
  the NoteText. Would have to have a solution to the multiple backref
  problem. One possibility is to have multiple up arrows and hide all
  but the targeted one with CSS, for one clean solution. Though I
  would hope that the browser's back button would be sufficient.

- Improve LinkData (should liveAt be some kind of http URL instead?)

- should tableHead and tableBody in SimpleTable have their own types
  (and attributes)?

- may want the Doc {title} field to be handled differently. Also may
  want to have the option of using the bibliographic title as the
  display title.

-}

-- * Documents and attributes

-- One of the recognized document types
data Doc b i = Doc
  { univAttrs :: UnivAttrs
  , title :: [i]
  , biblioAttrs :: BiblioAttrs
  , numberingAttrs :: NumberingAttrs
  , titlingAttrs :: TitlingAttrs
  , content :: DocContent b i
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

{- TODO HERE:

What do I need for bibliographic/meta attributes?

- title, authorship, origin for bibliographic storage and display
  - may need things like reported author affiliation statements at a
    later time. Oh, MODS includes affiliation in it. Handy.
  - would be handy if we could perform some kind of nice bibliography
    to display transformation, or maybe we should just have display be
    separate for now?

- for names, probably easiest to have fixed name transformations that
  take a given name and output a (mods-y) bibliographic version and a
  display version. Maybe by default we give the full term of address,
  say {name|{given|Edwin A.} {family|Abbott}}, and the bibliographic
  version would find the first {family} component and move it to the
  front (if not there already, putting in a comma if there was a
  transposition)? Giving -> Abbott, Edwin A.

- bibliographic title has: title, subTitle, partNumber, partName, nonSort

- display title is just normal [Inline], I think, but perhaps we
  should add parsers for the TitleParts. Might be worthwhile to think
  about the TitlePart structure, too.

-}

type NameURI = Text

data BiblioName = BiblioName
  { univAttrs :: UnivAttrs
  , nameType :: NameType
  , authority :: Maybe NameURI
  , content :: [NamePart]
  } deriving (Eq, Ord, Show, Read, Generic)

data NameType
  = PersonalName
  | CorporateName
  | ConferenceName
  | FamilyName
  deriving (Eq, Ord, Show, Read, Generic)

data NamePart = NamePart
  { univAttrs :: UnivAttrs
  , partType  :: NamePartType
  , content   :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data NamePartType
  = FamilyNamePart
  | GivenName
  | DateName
  | TermsOfAddress
  deriving (Eq, Ord, Show, Read, Generic)

data TitlePartType
  = MainTitle
  | SubTitle
  | TitleNonSort
  deriving (Eq, Ord, Show, Read, Generic)

data BiblioTitle = BiblioTitle
  { univAttrs :: UnivAttrs
  , content :: [BiblioTitlePart]
  } deriving (Eq, Ord, Show, Read, Generic)

data BiblioTitlePart = BiblioTitlePart
  { univAttrs :: UnivAttrs
  , partType  :: TitlePartType
  , content   :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data BiblioOrigin = BiblioOrigin
  { univAttrs :: UnivAttrs
  , place :: Maybe OriginPlace
  , publisher :: Maybe OriginPublisher
  , dateIssued :: Maybe OriginDateIssued
  , edition :: Maybe OriginEdition
  } deriving (Eq, Ord, Show, Read, Generic)

nullBiblioOrigin :: BiblioOrigin
nullBiblioOrigin = BiblioOrigin { univAttrs  = nullUnivAttrs
                                , place      = Nothing
                                , publisher  = Nothing
                                , dateIssued = Nothing
                                , edition    = Nothing
                                }

data OriginPlace = OriginPlace
  { univAttrs :: UnivAttrs
  , content :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data OriginPublisher = OriginPublisher
  { univAttrs :: UnivAttrs
  , content :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data OriginDateIssued = OriginDateIssued
  { univAttrs :: UnivAttrs
  , content :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data OriginEdition = OriginEdition
  { univAttrs :: UnivAttrs
  , content :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data BiblioAttrs = BiblioAttrs
  { titleInfo :: BiblioTitle
  , nameInfo :: [BiblioName]
  , originInfo :: BiblioOrigin
  , plainTitle :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data NumberingAttrs = NumberingAttrs
  { elemCounterRel :: Map ContainerName (CounterName, NumberConfig)
  , counterRel :: Map CounterName (Set CounterName)
  } deriving (Eq, Ord, Show, Read, Generic)

data NumberConfig = NumberConfig
  { numberStyle :: NumberStyle
  , refPrefix :: Maybe Text
  , refSep    :: Maybe Text
  } deriving (Eq, Ord, Show, Read, Generic)

data NumberStyle = NumberStyle
  { filterMethod :: ContainerPathFilter
  , displayTake :: Maybe Int
  , style :: LocalStyle
  } deriving (Eq, Ord, Show, Read, Generic)

data ContainerPathFilter
  = FilterByCounterDep
  | FilterByContainer ContainerName
  deriving (Eq, Ord, Show, Read, Generic)

data LocalStyle
  = DepthStyle [LocalNumberStyle]
  | AbsoluteStyle LocalNumberStyle
  deriving (Eq, Ord, Show, Read, Generic)

data LocalNumberStyle
  = Decimal
  | LowerRoman
  | LowerAlpha
  deriving (Eq, Ord, Show, Read, Generic)

data TitlingAttrs = TitlingAttrs
  deriving (Eq, Ord, Show, Read, Generic)

data DocContent b i
  = DocArticle (Article b i)
  | DocBook (Book b i)
  deriving (Eq, Ord, Show, Read, Functor, Generic)

data Article b i = Article
  { frontMatter :: [FrontMatter b i]
  , mainMatter  :: [Section b i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

data Book (b :: * -> *) i = Book
  deriving (Eq, Ord, Show, Read, Functor, Generic)

-- * Universal element attributes

data UnivAttrs = UnivAttrs
  { label :: Maybe Identifier
  , lang  :: Maybe Lang
  } deriving (Eq, Ord, Show, Read, Generic)

nullUnivAttrs :: UnivAttrs
nullUnivAttrs = UnivAttrs { label = Nothing, lang = Nothing }

newtype Identifier = Identifier
  { unIdentifier :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

newtype Lang = Lang
  { unLang :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

-- * Other attributes and data

-- When number markup becomes more complex, will want to make this
-- more complex.
newtype RawNum = RawNum
  { unRawNum :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

newtype PageTitle = PageTitle
  { unPageTitle :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

newtype LibUrlPart = LibUrlPart Text
  deriving (Eq, Ord, Show, Read, Generic, IsString)

newtype LibDomain = LibDomain Text
  deriving (Eq, Ord, Show, Read, Generic, IsString)

data LibUrl = LibUrl
  { domain :: Maybe LibDomain
  , path :: [LibUrlPart]
  } deriving (Eq, Ord, Show, Read, Generic)

data LinkData = LinkData
  { liveAt :: Maybe Text
  , num    :: Maybe NumberData
  , biblio :: Maybe BiblioData
  } deriving (Eq, Ord, Show, Read, Generic)

newtype NumberData = NumberData ()
  deriving (Eq, Ord, Show, Read, Generic)

newtype BiblioData = BiblioData ()
  deriving (Eq, Ord, Show, Read, Generic)

data RefData = RefData
  { liveAt :: Maybe Text
  , numberData :: NumberData
  } deriving (Eq, Ord, Show, Read, Generic)

data Void1 a
  deriving (Eq, Ord, Show, Read, Generic)

absurd1 :: Void1 a -> b
absurd1 = \case {}

-- * Sectional content

data SecAttrs i = SecAttrs
  { titleBody :: Maybe [i]
  , titleFull :: Maybe [i]
  , pageTitle :: Maybe PageTitle
  , num       :: Maybe RawNum
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

nullSecAttrs :: SecAttrs i
nullSecAttrs = SecAttrs { titleBody = Nothing
                        , titleFull = Nothing
                        , pageTitle = Nothing
                        , num       = Nothing
                        }

data Section b i = Section
  { univAttrs :: UnivAttrs
  , secAttrs  :: SecAttrs i
  , preamble  :: [b i]
  , children  :: [Subsection b i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

data Subsection b i = Subsection
  { univAttrs :: UnivAttrs
  , secAttrs :: SecAttrs i
  , content :: [b i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

data FrontMatter b i
  = Foreword UnivAttrs (SecAttrs i) [b i]
  | Dedication UnivAttrs (SecAttrs i) [b i]
  | Introduction UnivAttrs (SecAttrs i) [b i]
  deriving (Eq, Ord, Show, Read, Functor, Generic)

-- * Block content

-- type BlkRec f b i = f (Block b) (Inline i)

data Block (b :: * -> *) i
  = Bpar (Paragraph i)
  | Bformal (Formal (Block b) i)
  | Bcode BlockCode
  | Bolist (Olist (Block b) i)
  | Bulist (Ulist (Block b) i)
  | BsimpleTable (SimpleTable (Block b) i)
  | Bcustom (b i)
  deriving (Eq, Ord, Show, Read, Functor, Generic)

data MixedBody b i
  = InlineBody [i]
  | BlockBody [b i]
  deriving (Eq, Ord, Show, Read, Generic, Functor)

data Paragraph i = Paragraph
  { univAttrs :: UnivAttrs
  , content :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data Formal b i = Formal
  { univAttrs :: UnivAttrs
  , typ :: Maybe Text
  , num :: Maybe RawNum
  , titleFull :: Maybe [i]
  , titleNote :: Maybe [i]
  , titleSep :: Maybe [i]
  , content :: MixedBody b i
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data BlockCode = BlockCode
  { univAttrs :: UnivAttrs
  , content :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data Olist b i = Olist
  { univAttrs :: UnivAttrs
  , content :: [OlistItem b i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data OlistItem b i = OlistItem
  { univAttrs :: UnivAttrs
  , num :: Maybe RawNum
  , content :: MixedBody b i
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data Ulist b i = Ulist
  { univAttrs :: UnivAttrs
  , marker    :: ()
  , content   :: [UlistItem b i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data UlistItem b i = UlistItem
  { univAttrs :: UnivAttrs
  , content :: MixedBody b i
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data SimpleTable (b :: * -> *) i = SimpleTable
  { colSpec   :: ColSpec
  , tableHead :: SimpleTableHead i
  , tableBody :: SimpleTableBody i
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

newtype ColSpec = ColSpec
  { content :: [ColData]
  } deriving (Eq, Ord, Show, Read, Generic, Semigroup, Monoid)

data SimpleTableHead i = SimpleTableHead
  { univAttrs :: UnivAttrs
  , content :: [HeadRow i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

nullSimpleTableHead :: SimpleTableHead i
nullSimpleTableHead =
  SimpleTableHead { univAttrs = nullUnivAttrs, content = [] }

data SimpleTableBody i = SimpleTableBody
  { univAttrs :: UnivAttrs
  , content :: [BodyRow i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

nullSimpleTableBody :: SimpleTableBody i
nullSimpleTableBody =
  SimpleTableBody { univAttrs = nullUnivAttrs, content = [] }

newtype ColData = ColData Alignment
  deriving (Eq, Ord, Show, Read, Generic)

data Alignment = AlignLeft | AlignRight | AlignCenter | AlignDefault
  deriving (Eq, Ord, Show, Read, Generic)

data HeadRow i = HeadRow
  { univAttrs :: UnivAttrs
  , content :: [HeadCell i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data HeadCell i = HeadCell
  { univAttrs :: UnivAttrs
  , content :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data BodyRow i = BodyRow
  { univAttrs :: UnivAttrs
  , content :: [BodyCell i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data BodyCell i = BodyCell
  { univAttrs :: UnivAttrs
  , content :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data BlockControl b i
  = BcNoteText (NoteText (Block (BlockControl b)) i)
  | BcCustom (b i)
  deriving (Eq, Ord, Show, Read, Generic, Functor)

data NoteText b i = NoteText
  { univAttrs :: UnivAttrs
  , num :: Maybe Int
  , content :: MixedBody b i
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

-- * Inline content

type InlRec f i = f (Inline i)

data Inline i
  = Istr Str
  | Iemph (InlRec Emph i)
  | Iquote (InlRec Quote i)
  | Iname (InlRec Name i)
  | IworkTitle (InlRec WorkTitle i)
  | Iregularize (InlRec Regularize i)
  | Icitation (InlRec Citation i)
  | Iforeign (InlRec Foreign i)
  | IinlineMath InlineMath
  | IdisplayMath DisplayMath
  | Icode InlineCode
  | IpageMark PageMark
  | Iref Ref
  | ItitleComponent (InlRec TitleComponent i)
  | InoteMark NoteMark
  | Icustom i
  deriving (Eq, Ord, Show, Read, Functor, Generic)

data InlineControl i
  = IcRef SourceRef
  | IcNoteMark SourceNoteMark
  | IcCustom i
  deriving (Eq, Ord, Show, Read, Functor, Generic)

newtype Str = Str
  { content :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data Emph i = Emph
  { univAttrs :: UnivAttrs
  , content   :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

data Quote i = Quote
  { univAttrs :: UnivAttrs
  , content   :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

data Name i = Name
  { univAttrs :: UnivAttrs
  , content   :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

data WorkTitle i = WorkTitle
  { univAttrs :: UnivAttrs
  , content   :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

data Regularize i = Regularize
  { univAttrs :: UnivAttrs
  , old :: [i]
  , new :: [i]
  } deriving (Eq, Ord, Show, Read, Functor, Generic)

data Citation i = Citation
  { univAttrs :: UnivAttrs
  , target    :: Maybe Identifier
  , content   :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data Foreign i = Foreign
  { univAttrs :: UnivAttrs
  , content :: [i]
  } deriving (Eq, Ord, Show, Read, Generic, Functor)

data InlineMath = InlineMath
  { univAttrs :: UnivAttrs
  , content :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data DisplayMath
  = Formula UnivAttrs MathItem
  | Gather UnivAttrs Bool [MathItem]
  deriving (Eq, Ord, Show, Read, Generic)

data MathItem = MathItem
  { univAttrs :: UnivAttrs
  , num :: Maybe RawNum
  , isNumbered :: Bool
  , content :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data InlineCode = InlineCode
  { univAttrs :: UnivAttrs
  , content :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

data PageMark = PageMark
  { univAttrs :: UnivAttrs
  , content :: Text
  } deriving (Eq, Ord, Show, Read, Generic)

-- TODO: here. Think about what the _final_ Ref in the tree needs for
-- rendering, and anything else that would be wise to keep around in
-- the unfolded document. The data type below is the old one.

data Ref = Ref
  { univAttrs :: UnivAttrs
  , target :: LibUrl
  , refData :: RefData
  } deriving (Eq, Ord, Show, Read, Generic)

data TitleComponent i
  = TitleComponent TitlePart [i] [i] [i]
  deriving (Eq, Ord, Show, Read, Functor, Generic)

data TitlePart
  = TitlePrefix
  | TitleNote
  | TitleNumber
  | TitleSep
  | TitleBody
  deriving (Eq, Ord, Show, Read, Generic)

data NoteMark = NoteMark
  { univAttrs :: UnivAttrs
  , target :: Identifier
  , num    :: Int -- Ignoring style and other information right now.
  } deriving (Eq, Ord, Show, Read, Generic)

data SourceRef = SourceRef
  { univAttrs :: UnivAttrs
  , target :: LibUrl
  } deriving (Eq, Ord, Show, Read, Generic)

data SourceNoteMark = SourceNoteMark
  { univAttrs :: UnivAttrs
  , target :: Identifier
  } deriving (Eq, Ord, Show, Read, Generic)
