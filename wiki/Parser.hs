module Parser where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.FilePath as FilePath

import Grammar

parseString :: Parser LaTeX
parseString = do
  latex <- many parseParagraph
  eof
  return $ Paragraphs latex

parseParagraph :: Parser Paragraph
parseParagraph = do
  many newline
  texemes <- many1 parseTeXeme
  return $ TeXemes texemes


parseTeXeme :: Parser TeXeme
parseTeXeme =
  parseCommand <|>
  parseGroup <|>
  parseRaw <|>
  parseTeXSpecial

parseCommand :: Parser TeXeme
parseCommand = do
  char '\\'
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
  try $ string "begin{"
  name <- many1 alphaNum
  char '}'
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
  do { char '<'; return TeXCodeLt },
  do { char '>'; return TeXCodeGt }]

parseCodeBoxCommand :: Parser TeXCode
parseCodeBoxCommand = do
  char '\\'
  choice [
    do { string "li "; return TeXCodeLi },
    do { string "zi "; return TeXCodeZi }]

parseCodeBoxRaw :: Parser TeXCode
parseCodeBoxRaw = do
  raw <- many1 $
    (satisfy Char.isAlphaNum) <|>
    (satisfy (\ char ->
      elem char [
        ' ', ',', '.', '-', '=', ';', '(', ')', '{', '}', '[',']'
      ])) <|>
    try (do { newline; notFollowedBy (char '\\'); return ' ' })
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
  string "\\end{"
  string name
  char '}'
  return ()

parseEnd :: Parser TeXeme
parseEnd = do
  try $ string "end{"
  name <- many1 alphaNum
  char '}'
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
  char '{'
  paragraphs <- many parseParagraph
  char '}'
  return $ TeXGroup paragraphs

parseSpace :: Parser TeXeme
parseSpace = do
  char ' '
  return $ TeXRaw " "

parseRaw :: Parser TeXeme
parseRaw = do
  raw <- many1 $
    (satisfy Char.isAlphaNum) <|>
    (satisfy (\ char ->
      elem char [
        'ø','å','æ','Ø','Å','Æ'
      ])) <|>
    (satisfy (\ char ->
      elem char [
        '.',
        '!',
        '?',
        ',',
        ':',
        ';',
        '=',
        '(',
        ')',
        '[',
        ']',
        '/'
      ])) <|>
    do { many1 $ char ' '; return ' ' } <|>
    try (do { char '-'; notFollowedBy (char '-'); return '-' }) <|>
    try (do { newline; notFollowedBy newline; return ' ' })
  return $ TeXRaw raw

parseTeXSpecial :: Parser TeXeme
parseTeXSpecial =
  let
    parse = \ text constructor -> do
      try $ string text
      return constructor
  in do
    texSpecial <-
      parse "---" TeXEmDash <|>
      parse "--"  TeXEnDash <|>
      parse "``"  TeXOpenDoubleQuote <|>
      parse "''"  TeXCloseDoubleQuote <|>
      parse "`"   TeXOpenSingleQuote <|>
      parse "'"   TeXCloseSingleQuote <|>
      parse "<"   TeXLt <|>
      parse ">"   TeXGt
    return $ TeXSpecial texSpecial
