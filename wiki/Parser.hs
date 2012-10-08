module Parser where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Char as Char

import Grammar

parseString :: Parser LaTeX
parseString = do
  latex <- many parseParagraph
  eof
  return $ Paragraphs latex

parseParagraph :: Parser Paragraph
parseParagraph = do
  _ <- many newline
  texemes <- many1 parseTeXeme
  return $ TeXemes texemes

ensure :: Parser a -> Parser ()
ensure p = do
  _ <- p
  return ()

attempt :: Parser a -> Parser ()
attempt p = do
  _ <- try p
  return ()

parseTeXeme :: Parser TeXeme
parseTeXeme =
  parseCommand <|>
  parseGroup <|>
  parseRaw <|>
  parseTeXSpecial <|>
  parseComment

parseComment :: Parser TeXeme
parseComment = do
  ensure $ char '%'
  comment <- manyTill anyChar newline
  return $ TeXComment comment

parseCommand :: Parser TeXeme
parseCommand = do
  ensure $ char '\\'
  parseBegin <|>
    parseEnd <|>
    parseSpace <|>
    parseGenericCommand

parseGenericCommand :: Parser TeXeme
parseGenericCommand = do
  name <- many1 alphaNum
  optional space
  return $ TeXCommand name

parseBegin :: Parser TeXeme
parseBegin = do
  attempt $ string "begin{"
  name <- many1 alphaNum
  ensure $ char '}'
  case name of
    "codebox"     -> parseCodeBox
    "code"        -> parseCode
    "verbatim"    -> parseVerbatim "verbatim"
    _ -> do
      environment <- parseEnvironment name
      return $ TeXBegin environment

parseCodeBox :: Parser TeXeme
parseCodeBox = do
  code <- manyTill parseCodeBoxElement (try $ parseEndSpecial "codebox")
  return $ TeXCodeBox code

parseCodeBoxElement :: Parser TeXCode
parseCodeBoxElement = do
  optional (char '\n')
  choice [parseCodeBoxSpecial, parseCodeBoxCommand, parseCodeBoxRaw]

parseCodeBoxSpecial :: Parser TeXCode
parseCodeBoxSpecial = choice [
  do { ensure $ char '<'; return TeXCodeLt },
  do { ensure $ char '>'; return TeXCodeGt }]

parseCodeBoxCommand :: Parser TeXCode
parseCodeBoxCommand = do
  ensure $ char '\\'
  choice [
    do { ensure $ string "li "; return TeXCodeLi },
    do { ensure $ string "zi "; return TeXCodeZi }]

parseCodeBoxRaw :: Parser TeXCode
parseCodeBoxRaw = do
  raw <- many1 $
    satisfy Char.isAlphaNum <|>
    satisfy (`elem` " ,.-=;(){}[]") <|>
    try (do { ensure newline; notFollowedBy (char '\\'); return ' ' })
  return $ TeXCodeRaw raw

parseCode :: Parser TeXeme
parseCode = do
  code <- manyTill parseCodeLine (try $ parseEndSpecial "code")
  return $ TeXCode code

parseCodeLine :: Parser String
parseCodeLine = do
  optional $ char '\n'
  manyTill anyChar (char '\n')

parseVerbatim :: String -> Parser TeXeme
parseVerbatim name = do
  optional (char '\n')
  code <- manyTill anyChar (try $ parseEndSpecial name)
  return $ TeXVerbatim code

parseEndSpecial :: String -> Parser ()
parseEndSpecial name = do
  optional (char '\n')
  ensure $ string "\\end{"
  ensure $ string name
  ensure $ char '}'
  return ()

parseEnd :: Parser TeXeme
parseEnd = do
  attempt $ string "end{"
  name <- many1 alphaNum
  ensure $ char '}'
  environment <- parseEnvironment name
  return $ TeXEnd environment

parseEnvironment :: String -> Parser Environment
parseEnvironment name =
  case name of
    "enumerate"   -> return Enumerate
    "itemize"     -> return Itemize
    "definition"  -> return Definition
    _ -> fail $ "unknown environment \"" ++ name ++ "\"."

parseGroup :: Parser TeXeme
parseGroup = do
  ensure $ char '{'
  paragraphs <- many parseParagraph
  ensure $ char '}'
  return $ TeXGroup paragraphs

parseSpace :: Parser TeXeme
parseSpace = do
  ensure $ char ' '
  return $ TeXRaw " "

parseRaw :: Parser TeXeme
parseRaw = do
  raw <- many1 $
    satisfy Char.isAlphaNum <|>
    satisfy (`elem` "æøåÆØÅ") <|>
    satisfy (`elem` ".!?,:;=()[]/") <|>
    do { ensure $ many1 $ char ' '; return ' ' } <|>
    try (do { ensure $ char '-'; notFollowedBy (char '-'); return '-' }) <|>
    try (do { ensure newline; notFollowedBy newline; return ' ' })
  return $ TeXRaw raw

parseTeXSpecial :: Parser TeXeme
parseTeXSpecial =
  let
    parse' text constructor = do
      try $ ensure $ string text
      return constructor
  in do
    texSpecial <-
      parse' "---" TeXEmDash <|>
      parse' "--"  TeXEnDash <|>
      parse' "``"  TeXOpenDoubleQuote <|>
      parse' "''"  TeXCloseDoubleQuote <|>
      parse' "`"   TeXOpenSingleQuote <|>
      parse' "'"   TeXCloseSingleQuote <|>
      parse' "<"   TeXLt <|>
      parse' ">"   TeXGt
    return $ TeXSpecial texSpecial
