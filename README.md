# Scriba

A new extensible markup language, together with an intermediate format
and publishing tools suitable for new and historical documents.

## Syntax reference

A simple document is given here as illustration of the syntax, taken
from *An essay towards solving a problem in the doctrine of chances*
by the Rev. Thomas Bayes:

```
# mainMatter

& problem
  |{physPage|376} {emph|Given} the number of times in which an unknown
   event has happened and failed: {emph|Required} the chance that the
   probability of its happening in a single trial lies somewhere
   between any two degrees of probability that can be named.

## section
  & number|I
  & fullTitle|Section I

& definition
  ---
  & olist
    ---
    & li
      |Several events are {emph|inconsistent}, when if one of them
       happens, none of the rest can.
    & li
      |Two events are {emph|contrary} when one, or other of them must;
       and both together cannot happen.
    & li
      |An event is said to {emph|fail}, when it cannot happen; or,
       which comes to the same thing, when its contrary has happened.
    & li
      |An event is said to be determined when it has either happened
       or failed.
    & li
      |The {emph|probability of any event} is the ratio between the
       value at which an expectation depending on the happening of the
       event ought to be computed, and the value of the thing expected
       upon {reg{old|it's}{new|its}} happening.
    & li
      |By {emph|chance} I mean the same as probability.
    & li
      |Events are independent when the happening of any one of them
       does neither increase nor abate the probability of the rest.
```

## Language features

Currently the following elements are defined:

`p`
: Paragraphs. Can also be defined implicitly in paragraphed
  blocks. They contain inline elements.

`emph`
: Inline emphasized text.

`math`
: An inline mathematical formula or other simple construct. By default
  its content is written in TeX syntax.

`dmath`
: A displayed mathematical formula.

`gathered`
: A collection of formulas. This element contains a sequence of `line`
  elements, each containing mathematical text.

`physPage`
: A marker indicating that a physical page break occurred at or near
  the marker. Its content is any text, but the definition of this
  element will be modified so that a page locator term can be included
  (for linking to page images).

`name`
: A personal name.

`reg`
: Text regularization.

`ref`
: A reference to a numbered element in the document, roughly
  equivalent to the `\ref` construct in LaTeX. In rendered output it
  is replaced with the reference prefix, number, and suffix of the
  referenced element, according to the configuration of `ref` and that
  paticular element.

`title`
: The title of a work.

`cite`
: A citation. This element currently has no automatic behaviour.

`code`
: Inline code.

`codeBlock`
: A block of code, usually inside a `figure`.

`olist`
: An ordered list. The items of an ordered list are numbered, and so
  can be referenced. Ordered lists can also have types, but they do
  not do anything at the moment.

`ulist`
: An unordered list.

`formal`
: A titled, numbered block. Formal blocks can be given types, and the
  numbering and titling of a particular formal block type can be
  individually configured.

# Future directions

## Manual

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

## Meta handling

Need a consistent metadata handling policy, probably, since we will be
rendering bibliographies and citations. CSL compatibility would be
handy, I suppose, but all of these historical documents will probably
need weird bespoke citation schemes. Still, a CSL-like derivation
mechanism would be welcome, since shared citation elements are at
least somewhat common.

## Standalone rendering

If we are to keep the general standalone setup (and not integrate it
as a special case of the [library](#libraries) construct), then we
need to

## Libraries

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

## Style

CSS has :lang selector, so we can have styles based on language
(e.g. differing {title} styles and whatnot).

## New elements, attributes, behaviours, fixes

- Global permissible attributes on everything (`id`, `lang`, whatever
  else we decide to have).

- a root `document` section? If we have editorial note support then
  we'll need some way of distinguishing the two, if they are to appear
  together in the document. Might be helpful for composite documents
  as well.

- Notes (foot or end according to rendition), possibly including
  editorial notes. Different varieties of notes would need to be
  numbered and displayed differently, of course. The mechanism would
  most likely be through `noteMark` and `noteText`.

- Figures. Similar to formal blocks, except they have a block caption
  (that will by default be suffix). The caption can have a prefix and
  number, like formal blocks.

- Tables. There should be a top-level `table`, in addition to whatever
  mathematical tableau functionality we provide.

- citation components (authors, editors, publishers, events, and so
  on), and automatic citation generation. Related to [meta
  handling](#meta-handling) and [libraries](#libraries).

- generated content, like a table of contents, lists of content,
  bibliographies.

- paginated output (web-paginated, that is).

- a TeX/LaTeX/ConTeXt writer.

- consider removing the `conclusion` section of formal blocks.

- generally fix up the current style configuration in documents: allow
  custom lists types, improve numbering and number style configuration.

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
