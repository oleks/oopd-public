module Grammar where

data TeXSpecial
  = TeXEmDash
  | TeXEnDash
  | TeXLt
  | TeXGt
  | TeXOpenSingleQuote
  | TeXCloseSingleQuote
  | TeXOpenDoubleQuote
  | TeXCloseDoubleQuote
  deriving(Show)

data TeXCode
  = TeXCodeRaw String
  | TeXCodeLt
  | TeXCodeGt
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
  | TeXComment String
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
