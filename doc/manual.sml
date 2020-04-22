{scriba
  {title|The scriba markup language and document system}
  {type|article}}

Scriba is a plain text markup language emphasizing extensibility,
clarity, and ease of use. It is also a compiler, taking source scriba
documents and rendering them to multi-page HTML, EPUB, and TeX. The
format includes flexible markup constructs designed to represent a
range of historical scientific material, but these can be used equally
well for new publications.

# {title|An introduction to the language}

## {title|Paragraphs}

The bulk of your document will most likely be paragraphs, sequences of
normal text and marked-up content representing a single idea in your
writing. A paragraph in scriba contains {emph|paragraph text} and
{emph|elements}. Two sample paragraphs are given below:

&{codeBlock {language|scriba}`
  There was nothing so very remarkable in that; nor did Alice think
  it so very much out of the way to hear the Rabbit say to itself,
  {q|Oh dear! Oh dear! I shall be late!} (when she thought it over
  afterwards, it occurred to her that she ought to have wondered at
  this, but at the time it all seemed quite natural); but when the
  Rabbit actually {emph|took a watch out of its waistcoat-pocket},
  and looked at it, and then hurried on, Alice started to her feet,
  for it flashed across her mind that she had never before seen a
  rabbit with either a waistcoat-pocket, or a watch to take out of
  it, and burning with curiosity, she ran across the field after it,
  and fortunately was just in time to see it pop down a large
  rabbit-hole under the hedge.

  In another moment down went Alice after it, never once considering
  how in the world she was to get out again.
`}

Plain paragraph text is any span of unicode characters other than
{code`\`}, {code`{`}, or {code`}`}, with the additional restriction
that paragraph text cannot contain blank lines; like in the example
above, these signal the end of the paragraph. The sequences
{code`\\`}, {code`\{`}, and {code`\}`} are used to represent those
literal characters, should you need them in text.

The other feature of paragraph markup is {emph|inline elements}, which
are used to give further meaning to your text and to control scriba's
many features.{%TODO: link here when this section is writtten%} Simple
elements start with {code`{tag|`} and end with {code`}`}. The
paragraphs above have a {code`q`} element, representing quoted text
and likely to be rendered between quotation marks, and a `{code`emph`}
element, representing emphasized text and likely to be rendered in
italic type.

## {title|Sections}

{% TODO: add this back in

# {title|A sample document}

The following is a skeleton scriba document.

{%TODO: put this in a {figure} and reference it in the sentence above%}

&{code {language|scriba}`
  {scriba
    {title|Sample document}}

  # frontMatter

  <introductory sections>

  # mainMatter

  <main narrative>

  # endMatter

  <concluding sections>
`}

The first component of a scriba document is an optional
{code`{scriba}`} element containing document attributes, like the
title, dates, authors, or editorial notes.

%}