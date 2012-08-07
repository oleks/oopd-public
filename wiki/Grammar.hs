module Grammar where

import qualified Data.List as List

data TeXSpecial
  = TeXEmDash
  | TeXEnDash
  | TeXOpenSingleQuote
  | TeXCloseSingleQuote
  | TeXOpenDoubleQuote
  | TeXCloseDoubleQuote
  deriving(Show)

data TeXCode
  = TeXCodeRaw String
  | TeXCodeLi
  | TeXCodeZi
  deriving(Show)

data Environment
  = Itemize
  | Enumerate
  | Definition
  deriving(Eq, Show)

data TeXeme
  = TeXRaw String
  | TeXSpecial TeXSpecial
  | TeXVerbatim String
  | TeXCodeBox [TeXCode]
  | TeXCode [String]
  | TeXBegin Environment
  | TeXEnd Environment
  | TeXCommand String
  | TeXGroup [Paragraph]
  deriving(Show)

data Paragraph
  = TeXemes [TeXeme]
  deriving(Show)

data LaTeX
  = Paragraphs [Paragraph]
  deriving(Show)

data Counter
  = ListingCounter
  | DefinitionCounter
  | SectionCounter
  deriving(Eq,Show)
