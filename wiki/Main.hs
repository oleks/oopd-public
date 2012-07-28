module Main(
  compileFileToHtml
) where

import Text.Parsec
import Text.Parsec.Text

import System.FilePath(
  FilePath, addExtension, dropExtension, joinPath)
import Data.Time(getCurrentTime)

import Data.Text hiding(map)

import Grammar
import Parser
import Compiler

parseLaTeX :: Text -> Either ParseError LaTeX
parseLaTeX latex = parse parseString "LaTeX input" latex

parseFile :: FilePath -> IO (Either ParseError LaTeX)
parseFile filePath = do
  input <- readFile filePath
  return $ runParser parseString () filePath (pack input)

{- TODO: The interfaces of parseFile/compileFileToHtml are not compatible wrt.
paths. -}

compileFileToHtml :: FilePath -> IO (Either ParseError ())
compileFileToHtml inFilePath = do
  let (inPath, outPath) = getPaths inFilePath
  result <- parseFile inPath
  case result of
    Right latex -> do
    {
      html <- compileAux latex;
      writeFile outPath html;
      return $ Right ()
    }
    Left error -> return $ Left error

getPaths :: FilePath -> (FilePath, FilePath)
getPaths inFilePath =
  let
    outFilePath =
      addExtension (dropExtension inFilePath) ".html"
    inPath = joinPath ["latex", inFilePath]
    outPath = joinPath ["html", outFilePath]
  in
    (inPath, outPath)

compileFile :: FilePath -> IO String
compileFile filePath = do
  result <- parseFile filePath
  case result of
    Right latex -> compileAux latex
    Left errorMessage -> error $ show $ errorPos errorMessage

compileAux :: LaTeX -> IO String
compileAux latex = do
  utcTime <- getCurrentTime
  return $ compile utcTime latex
