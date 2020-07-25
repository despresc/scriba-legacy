module Text.Scriba.Package where

{-
Packages

- have a default linkage resolver (a library)

- list the documents they depend on, and optional resolver overrides
  (version bounds?), and extra document dependencies (sha pinning?)

- internally, have some kind of URI scheme for addressing
  components of libraries (and so documents)

(could have textual imports, but would require DAGness for imports -
probably want to list textual dependencies separately)

As for building

- for any document, compilation can happen up to the intermediate
  (pre-linking) phase on its own

- so we need (at least) two build targets for packages: the first is
  an intermediate compilation that gets us to the (linkage,
  intermediate, linkage-requirements) phase; the second is the final
  compilation that takes all of the gathered linkage information and
  returns the required outputs (the unfolded document, perhaps, and
  the supported rendered outputs).

- some kind of resolver override for libraries? e.g. if you want to
  clone the library and build the whole thing yourself, you should be
  able to say "actually, the resolver should be this library itself".

Note that the linkage information should be a tree, something like

data LinkNode = LinkNode LinkData Linkage
type Linkage = Map Identifier LinkNode

The link data represents the metadata capabilities of the particular
node. A document can have bibliographic metadata, internal elements
can have numbering or live linking data, that sort of thing. You can
mix the different types, too (e.g. something can have bibliographic
data but not appear live anywhere, I guess)

can we do identifier:version for version selection?

need to think about work/instance/item, I suppose. Linking probably
wants to be done on the instance level, maybe with fallbacks. I
think. That means you should be able to address various sections of
the library, right? We could have

libscb://library/item/...

and

libscb://library/work/...

but perhaps that commits to too much? Probably shouldn't have
identifier namespacing like that.

If we commit to everything being a document (or everything being
purely capability-based), then we can have

libscb://library/<identifier>/...

without committing to any particular naming scheme. E.g. one library
could have a flat scheme with certain reserved identifiers like `name`
for name authority files, if they like. Or `data` for more abstract
data files (like wikidata), if these things become relevant to the
digitizing.

Is everything simply linkage information? Not sure if there ought to
be anything more.

Need types in the linkage? I suppose it should return a list of (type,
value) capabilities when requested.

-}

import           Development.Shake
import           Development.Shake.FilePath

{-

Simple bibliographic information:

- [TitleInfo] with title, subTitle, nonSort

- [Name], with roles, authorities, etc.

- [OriginInfo]

- some kind of (Set Tag)

-}

data BiblioData = BiblioData
