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

data TeXeme
  = TeXRaw String
  | TeXSpecial TeXSpecial
  | TeXVerbatim String
  | TeXCodeBox [TeXCode]
  | TeXCode [String]
  | TeXBegin String
  | TeXEnd String
  | TeXCommand String
  | TeXGroup [Paragraph]
  deriving(Show)

data Paragraph
  = TeXemes [TeXeme]
  deriving(Show)

data LaTeX
  = Paragraphs [Paragraph]
  deriving(Show)
