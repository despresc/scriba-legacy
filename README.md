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
