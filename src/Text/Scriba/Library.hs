module Text.Scriba.Library where

{-

- A simple library configuration is a directory of documents, each
  with a manifest file in an easily-discoverable place. The manifest
  would identify the main file of the document (if present) and
  probably include the document metadata and other properties. Even
  just package-name.<some suffix> or manifest.scb. Something like
  that.

- Are documents expected to list their dependencies explicitly? If
  they're supposed to be versioned separately, then that's probably a
  good idea. On the other hand, that _is_ pretty
  un-ergonomic. Certainly the library itself can't have a version,
  other than possibly for major structural things. But if the library
  is the major unit of communication then it has to be that way! I
  suppose there could simply be moderately infrequent updates? A
  once-a-month sort of thing. Okay.

- So the library is completely release-agnostic and should build,
  precisely and reproducibly, what it is given.

-}
