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

- Guarantee identifier uniqueness in the document.

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

- Global permissible attributes on everything (`id`, `lang`, whatever
  else we decide to have).

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