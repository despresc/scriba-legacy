# Actual TODO

- Move `Element.Memoir.Heading` to its own module. Also consider
  absolute header definitions, since there is going to be a set
  pagination structure with our restricted sections.
- Add an `Element/ElemNumber` module and put a common source number
  parser in it (and take out the individual ones from the numbered
  elements). Or, add a parser for it to `Decorate/Common`, and move
  the `Element/Identifier` parser there too.
- Document that source-defined numbers do not save the numbering
  configuration of their container, so the rendering of `ref` may be
  surprising if one expects, e.g., the prefix of the container to be
  displayed. Also consider changing this so that that information _is_
  saved. Also document that elements with source-defined numbers still
  get numbered!
- Guarantee identifier uniqueness in a document by modifying
  `Gathering` to check for the presence of identifiers before adding
  them to the map.
- Add an `Options` module (or sequence of modules) somewhere and put
  the attribute/opting parsing and definitions there.
- Fix the error message that is thrown on an unrecognized/unexpected
  element, so that it preserves the presentation of the
  element. Important for paragraphs and other sugar.
- Add `SecAttrs` to `Element.Memoir.FrontMatter`
- Add a `NodalAttrs` (or something like it) with plain text titles for
  sections, suitable for going in a url (for eventual
  pagination). Remember, for pagination I need to: determine which
  sections should be nodes; figure out the url names of the relevant
  sections; report the page on which each element occurs in a document
  in the linking step; fix all the links so that they point to the
  right places.
- Create an HTML identifier rendering function in, say,
  `Element/Identifier`, then use it to annotate HTML elements, instead
  of whatever ad-hoc things we do now. Example in the rendering of
  `Element/List` and the `NoteText` rendering in `Markup`, but applies
  to everything with an `Identifier` in it.
- Move the components of `Element/DocAttrs` related to decoration and
  put them in `Decorate/Common`, along with the functions that run the
  decorations.
- Consider moving `MemDoc` into `Markup` (relates to monomorphizing
  the other types).
- Look into monomorphizing `Inline` and `Block`, and creating separate
  inline and block types for different compilation stages (will
  require changes to `Decorate`). Example: the `NoteText` should have
  a `NumberAuto` after numbering, and should have no number before.
- Add custom block and inline support back to `Inline` and
  `Block`. Potentially related to monomorphizing those types, but can
  be applied separately.
- Selectively render empty elements
- Fix element numbering configuration in light of the restricted
  sectioning
- Document `Intermediate/`
- Document `Decorate/`, observing that linking _must_ come after titling. 
- Document `Element/`
- Document `Markup/`

# Project structure

The main task here is to find a permanent home for what follows in
this section, and to then ensure that the project follows the
structure.

## General description

At the highest level, we have the library concept itself, which is a
collection of documents that all should compile together, and can
reference each other. Libraries must, therefore, define their contents
in some way, most simply via a config file with a list of document
sources in them (maybe with globbing?). Libraries could, additionally,
define various library-wide things like header files, tikz files, that
sort of thing. Other things like image stores could be possible,
though that might be handled via a special document type (a picture
book). Other things, like inter-library linking, may also be possible.

The next level is the document. A salient example is a scriba
document, which both _requires_ that external references be defined,
and _provides_ anchor points for external reference. The "opaque
document", containing a meta page and optional identified attachments,
is another document possibility, to provide for a form of bibliography
store (bibliographic entry with no attachments), image store (image
with metadata), among other things. Documents themselves can be
referenced too (which might want to be a kind of citation), and so
documents in libraries should have library-unique names for reference
purposes.

Libraries can be rendered, so to speak. This involves first going over
each source document, determining its linkage capability (identifiers
inside it and how they may be referenced) and its linkage requirements
(references inside it to other identified things in the library), then
using that collected linkage capability to resolve the references
inside documents. There should be a strict and lax mode for library
compilation, the latter collecting a list of broken references and
inserting some fallback element in their stead. The whole process
should cache the intermediate compilation results (recompilation of
documents being triggered whenever its linkage data, in or out,
changes).

## Library and document compilation

For a library:

- The library determines all of the documents contained within it,
  including externally-linked libraries (as a kind of phantom library
  document, I suppose?), and their types.

- Parse each document (if applicable), resolving its simple local
  imports, numbering it, and collecting its linkage information. At
  this point we might want to know the document pagination so that we
  can pass along absolute paths in the linkage information, but this
  isn't strictly necessary, since we can fill that information in
  later.

  An entry in the linkage capability information includes:

  - the fully-qualified identifier of the element
  - optional numbering data, if a numbered element
  - optional bibliographic data, if a citable (?) element. Might apply
    only to documents themselves, unless we have a bibliographic entry
    element.

  An entry in the linkage requirement information should include a
  fully-qualified identifier and an indication of what the it requires
  (mere presence in the case of a simple link, numbering data in the
  case of a `ref`, bibliographic data in the case of a `cite`).

  We might want to have some nice fallback for missing references, if
  possible (location of the document cited and the claimed identifier?
  though that only works if we do not allow entirely missing
  documents).

- Validate the linkage information, updating the notices of linkage
  degradation, if applicable

- Resolve the references inside documents with the collected linkage
  information. This whole process might be easier with a more flexible
  `Ref` element that can tolerate missing identifier targets.

For scriba documents:

- parse the document with `Source/Parse`

- convert the source document into an intermediate `Node` one with
  `Intermediate/Node`.

- use the definitions and parsers in `Element/` to convert the `Node`
  into a recognized document type

- number and title the document

- gather the linkage information from the document and pass it up to
  the library

- receive the combined linkage information from the library and fill
  in the references in the document

- render the document in the available output formats.

This leaves some questions open:

- how is the structure of a document defined? One way might be through
  a document meta file, like a cabal file, listing the source files,
  including special build instructions (in case images need to be
  compiled, for instance), bibliographic data, and other document
  properties. Of course, this could be done in a single file, but it
  might be clearer to have the option to separate them when in a
  library. Would that require a set (possibly per-library) document
  meta file location? Something like a single `filename.scb.meta` file
  in that directory?

- what is the link/reference/citation syntax inside documents?
  Everything is potentially partially qualified, so there may need to
  be link resolution. Also a consideration is reference by number,
  though that might require that numbered things have an identifier
  based solely on their number and type.

- the timing of (web) pagination should be defined. If we allow
  references to the physical pages of things in the print output then
  the timing of that pagination will need to be defined as well.

Note that external books themselves should be cached (or there should
be an option to do so), including any repo history (if applicable).

## The repository, application, and type structures

(Write out the requirements of the `Doc` data type, the metadata type
requirements, how different sources of metadata should be combined,
how this repository should be organized in light of the previous
sections, what should go in other repositories, how the scriba
application ecosystem should be organized).

# Hyperlink degradation

Since we want to allow the printing of documents, we must also come up
with somewhat robust ways for hyperlinks to degrade in LaTeX paper
output. For within-document links we can simply use the page number of
the element in question in some way, and for links external to the
library we can print the url as-is. Links to things outside the
document but inside the library might be able to use the page number
of the element, if there were a primary physical output configuration
defined for each document, since we could go through the `aux` files
of each document and collect the labels that are defined in each.

Hyperlink degradation really needs to be considered alongside the
link/ref/cite design and linkage capabilities, of course. We might not
want to allow references to external documents, only citations, though
the reference data of external documents could still be used in the
locator terms. So `{cite@libraryBook#anInterestingTheorem}` might be
rendered as `Theorem 2.7 of [5]`, given the right configuration. Of
course, for digitized books this might all be irrelevant, if we simply
give the visible text of such citations, or at least the visible
locator terms.

# Pagination, linking

We should be able to define the pagination of documents in HTML
rendering, I think, so that the amount of content per served page is
reasonable. This can be within the document, or perhaps outside of the
document as well (so that multiple pagination schemes can be used at
once on a website). Pagination could be defined through simple
matching on section types, perhaps?

There is a small issue with block content followed by subsequent
paginated subsections. I suppose that we could simply forbid such a
situation? Or, we could instead have pagination apply to the content
of sections, so that in, say, *Disquisitiones Arithmeticae*, you might
paginate by `frontMatter`, `mainMatter`, `backMatter`, which might be
the default pagination scheme for books. In that particular book, of
course, there was the problem of the one particularly large section,
which had major article groupings inside it. If we paginate instead by
`sectio`, then that one would be split up into a number of article
group pages, followed by two major article group pages.

There is still the problem of the containing sections, of course.

What about page breaks? We could provide a list the section types, and
these would have page breaks immediately before them. This creates the
(potentially) undesirable situation of not having any parent titles on
the pages of the children; you might expect `Chapter 7 \\ Section 1.`
on a section page, and not merely `Section 1.`. We would need to
filter out "empty" pages: pages without any preamble content. Either
that, or *all* upper pages get their own pages. Something like
`Title. \\ Preamble \\ Table of subsections`?

This would be easier if we had some kind of uniform book structure, of
course. Could simply have an "articulus" division, for instance, for
Gauss and related books, that's simply numbered and has no further
sectional content. Maybe a generic unnumbered section type? Either
that or switch to milestones for those groupings and have the TOC
somehow be aware of them. Then we might have a `secGroup` for grouping
sections together.

Let's say a section is not-necessarily-paginated, and it contains a
paginated child (as a descendant). Then it itself must become
paginated. Fine. For each section we decide if individual children
should be on their own pages. If any child is on its own page then all
the children must go on their own pages. Or perhaps all subsequent
children? We'll do all children for now.

```
Title
-----

+------------------+
|     Content      |
| (child sections) |
+------------------+

Section preamble
```

and the `Content` will not be present if there are no paginated child
sections.

Note that every node-like element (books, parts, chapters, potentially
sections, in addition to the documents themselves) must have a plain
text title, since we need something to use in the URL and title of the
relevant web page.

# Manual

- generally rework the introductory section, so that it is gentler,
  less technical.

- mention that paragraphs can't start with &

- link to the inline element section in the paragraph section

- link to amsmath, sections on numbering and referencing, maybe
  whatever manual section discusses mathematics in more detail.

- link to the block section's discussion of block verbatim content

- some kind of codeFigure custom block?

- Move the inline verbatim discussion to a section on inline elements?
  The mention of the start-of-paragraph & issue could be included
  there too.

- for the element reference section, should have sample usage, a
  precise discussion of permitted usage, interactions with the various
  systems, and so on. Perhaps some kind of data table format for parts
  of it? Might be a good fit for templating in some cases:
  automatically generated data sheets from the parsers; having a
  template take some code and put it in a figure alongside a rendered
  example of the code, but perhaps that is too fancy to have in the
  code document system. For the former, could have a method to import
  tables, and then simply make sure they're generated as needed.

  Other templating possibilities: turning something like `## $secElem
  @ code` (syntax and definition to be determined) into `## subsection
  {id|elem-code} {title|code}`. Could have an element reference
  construct, so that `{$elemRef@code}` becomes
  `{ref@elem-code|{code|code}}`, though that might be better-served by
  having a custom section type with its own reference rendition
  behaviour? Or we should just have the titles of those sections be,
  say, `{title|{code|code}}`.

- the element reference section might want to be an appendix.

# Meta handling

Need a consistent metadata handling policy, probably, since we will be
rendering bibliographies and citations. CSL compatibility would be
handy, I suppose, but all of these historical documents will probably
need weird bespoke citation schemes. Still, a CSL-like derivation
mechanism would be welcome, since shared citation elements are at
least somewhat common.

For now, we should implement a simple standalone digitized article
style, that might, say, include the relevant bibliographic details at
the start of the article, under the title. This might involve a simple
"sources of:" followed by a list of rendered sources? We might not
even want to have that _in_ the page, though.

# Standalone rendering

If we are to keep the general standalone setup (and not integrate it
as a special case of the [library](#libraries) construct), then
various aspects of it should be improved:

- better math configuration and handling (possible pre-rendering,
  using KaTeX instead of MathJax). For math in particular it may
  become necessary (with compound documents) to either propagate TeX
  math macro definitions upward (to be included in the header) or to
  _stop_ doing such propagation and instead define them with
  `newcommand`s inside the documents themselves.

- generally allowing header files when compiling output (to use custom
  css or TeX headers)

- good web pagination, including inter-document link adjustments.

# Project structure and code improvements

- Split up `Element/Doc` and `Element/Section` into multiple
  pieces. In particular, we may want the `DocAttrs` to be taken out of
  `Doc` and renamed, depending on how composite documents and document
  metadata will ultimately need to be handled. Also move the remaining
  `Markup` parsers into the elements themselves, perhaps adding an
  `Element/Common` module.

- Reduce the number of `getSourcePos` invocations in
  `Source/Parse`. Some parsers (like the `InlineWhite` parsers) can be
  consolidated, and single calls can be made before attempting to
  parse individual nodes.

- Clarify whitespace policy. Right now none of the `Element` parsers
  allow whitespace around elements when recognizing them. This is fine
  when the output comes from a parsed scriba document, but maybe we
  don't want to rely on that? That also means that elements with
  unusual presentations might not be parsed correctly.

- Consider switching to a more restrictive parser model. This depends
  on exactly what kind of structures we allow, but it might be better
  to have a parser data type (or class) that can tag element parsers
  with expectations and generally handle expectation and error
  propagation better than what is currently done. We might want
  classes like `FromNode` and `FromElement` as well, which would
  simplify some things. One potential example: in `pSectionContent`,
  if we encounter an unrecognized block `block`, we get the error
  `expecting one of: section`, because the `manyOf $ pBlock pInl` will
  fail and not not propagate its expectations forward.

- Errors need to be tested and improved, in general. For source, we
  should also have error regions, so that we can report what was being
  parsed at the time the parse error occurred (useful for malformed
  comments, improperly braced elements, and so on).

- The document structure parsing needs to be improved. The explicit
  matter structure should be: optional `frontMatter`, optional
  `mainMatter`, optional `backMatter`, then the end of content. At the
  moment all three must be present for the document to have that
  structure. This might require some use of `whileParsing`.

- Have a `whileParsing`-type error that does not require an explicit
  `SourcePos`? Or modify that error so that a description can be
  substituted for a source position.

- Possibly unify element type configuration, so that you would
  configure a `theorem` according to its possible source presentations
  (as a formal block, list, section, if applicable). Not sure if this
  is more or less complex than the current system.

- the numbers we report in the numbering data should probably include
  the counter state that was used to create them.

- Have different `Inline` and `Block` types representing different
  compilation stages, instead of only parametricity? Stages could
  strip out different branches of the inline types. Could get too
  unwieldy, though. This might also require better class derivation
  (for rendering, deriving monadic properties, and so on).

- Have the `Inline` type be `Inline f a = Iemph (Emph (f a))`? Or
  something similar. Might want to look into recursion scheme
  compatibility.

- Turn `Inline` and `Block` into monads?

- Generally improve documentation and code organization. It's gotten
  messy.

- Fix up the current style configuration in documents: allow custom
  lists types, improve numbering and number style configuration.

- Attribute exclusivity? Would want to run all the attribute parsers
  and return an ambiguity error if more than one matches. Relates to
  unknown attribute errors too (since we have none).

- Enforce a non-empty title in a document? Or just emit a warning?
  Could have a strict mode that turns it on.

- Modify the container-counter relation compilation errors so they fit
  better with the current scriba errors.

- Some kind of "container name" class? Would be helpful for sectioning
  and titling.

# Libraries

I envision a library being a collection of documents that can be
linked to each other, referenced from other libraries and documents,
and imported (in whole or in part) into other libraries. This could be
extended: someone could have a personal `wiki` document that can link
to other documents in their library (or in external libraries). Need
to research build systems, archival practices, since we will
(potentially) want local caching (of metadata, including linkage
information, and of the documents themselves) and more direct
importing ("borrowing" from a library, "rebinding" and other
derivation mechanisms).

Libraries wouldn't have to be of scriba documents entirely. As long as
we had a way to detect (or specify, or add to) the capabilities of
documents, the library behaviour could degrade nicely. E.g. you could
have a pdf with a separate metadata file, or a pure metadata document
(for use in referencing). Perhaps this will provide a mechanism for
image handling? Could have a "book of pictures" document type that can
be referenced from elsewhere (if not a simple shared image pool).

Libraries could have a "house style" that defines the (possibly
complex) css and latex that the build system will output, among other
things (default pagination levels, citation and reference schemes).

These would have to compile well to a print format. Need good
fallbacks for purely link-defined constructs, though it shouldn't be
too hard for historical documents (which were written with print
references in mind).

Metadata would need to become more complex to handle this. One would
expect to be able to define subset relationships and composite
documents (a journal full of articles, an encyclopedia of volumes, or
more complex relationships like adding commentary on a work, parallel
translations).

# Style

CSS has :lang selector, so we can have styles based on language
(e.g. differing {title} styles and whatnot).

# New elements, attributes, behaviours, fixes

- name authority files (see <http://id.loc.gov/authorities/names.html>
  and, e.g., <http://id.loc.gov/authorities/names/n85044155.html>) for
  linking names and locations. Would be entries in a special book of
  names for me, I suppose.

- consider adding `data-` attributes to things, to show what they
  represent. Requires more research.

- Global permissible attributes on everything (`id`, `lang`, whatever
  else we decide to have).

- auto-identification of elements? There is numbering-based
  identification and "stable" identification (based on the first bit
  of plain text content in the container). The former is good for
  documents that are not expected to change very much.

- A root `document` section? If we have editorial note support then
  we'll need some way of distinguishing the two, if they are to appear
  together in the document. Might be helpful for composite documents
  as well.

- Notes (foot or end according to rendition), possibly including
  editorial notes. Different varieties of notes would need to be
  numbered and displayed differently, of course. The mechanism would
  most likely be through `noteMark` and `noteText`.

- Extend the `ref` mechanism to allow for multiple references, and
  gathered references, so that `ref@thing1 thing2 thing3 thing4` would
  become `eqns. 1-3, 7`, or something similar.

- `ref` configuration to allow prefix and suffix content to be added
  (like parentheses or square brackets), and to define different
  prefix forms (title/lower case, plural forms) which would be
  controlled by local `ref` invocation customization. Also allow
  things like prefix suppression, possibly number style overrides when
  referencing linked documents, other such improvements.

- Figures. Similar to formal blocks, except they have a block caption
  (that will by default be suffix). The caption can have a prefix and
  number, like formal blocks.

- A linking construct in formal blocks (and perhaps other elements)
  indicating some kind of topic or relatedness. Say that we have a
  proof *of* a theorem, or a discussion *of* a proposition. We might
  want to render a link to the thing being proved or discussed. Need
  to think about how that gets rendered.

- Simple local imports. Distinct from linkage and inter-library
  borrowing. Something like `{import@url}`, with similar syntax for
  block and section imports. Should probably have a safe mode to
  restrict the allowed urls, perhaps resolving paths relative to the
  root of the project, if that makes sense. This would require more
  control elements, internally. Might want different types of imports,
  like verbatim imports, simple table imports (with different table
  styles). Would need to specify a "main" or "index" document if they
  were allowed.

- Tables. There should be a top-level `table`, in addition to whatever
  mathematical tableau functionality we provide.

- citation components (authors, editors, publishers, events, and so
  on), and automatic citation generation. Related to [meta
  handling](#meta-handling) and [libraries](#libraries).

- generated content, like a table of contents, lists of content,
  bibliographies.

- inline ordered lists

- unordered list item configuration (marker styles), and unordered
  list/list item/list attributes (identifiers and things).

- paginated output (web-paginated, that is).

- a TeX/LaTeX/ConTeXt writer.

- consider removing the `conclusion` section of formal blocks.

- A generic `span` and `div`? Would only be useful if we had global
  attributes.

- equation numbering "contexts"? Would like to have a
  {startEqnContext}, perhaps, that would influence how the numbering
  is applied to equations, since these things can become fairly
  complex.

- section precis.

- epistolary support? salutations and valedictions (for dedications,
  introductions, prefaces, and other such sections), with physical
  address lines (or parts).

- syntax for boolean attributes? Currently we're using the mere
  presence certain attributes, like `noNum`, as "true".

- Allow more exotic numbering and titling. Need custom number
  separators, better numbering style configuration (so that path
  filtering is a little more unified and the associated style
  behaviours are a little more unified), moveable title parts,
  suppressed number rendering (but still have things be numbered
  internally) and editorial numbering.

- richer title components? subtitles and intermediate titles, title
  break locations and title lines, internal title separators.

- for `physPage`: identifiers for different sources of page images?
  especially relevant for combined editions.

- add automatic physPage identifiers for linking purposes.

- footnote numbering configuration.

- paragraph numbering? Very helpful for digitized editions that are
  not expected to change much. Would insert a pilcrow link anchor, I
  imagine. Actually, would be nice to have a switch to turn all the
  suppelementary anchors on and off too.
  
- some kind of preference for translated editions? or discovery
  feature ("this is available in such and such a translation").

- look into automatically inserting anonymous section breaks before
  sections that have no title. More specifically, they would be added
  between sections if the first child of the current section that has
  a non-empty `preamble` and none of the sections in between have a
  title. This *may* be required for the `*Matter` sections, but for
  the others we could instead have an attribute that turns it on (via
  a class).
