module CodeCompiler(compileCode) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable
import Text.Show

import Grammar

data Context
  = Context {
    output :: [String],
    lineNumber :: Int
  }

initialContext :: [String] -> Context
initialContext output
  = Context {
    output = output,
    lineNumber = 0
  }

data Code t = Code {
  runCode :: Context -> (t, Context)
}

instance Monad Code where
  return value = Code { runCode = \context -> (value, context) }
  code >>= f = Code { runCode = \context ->
    let
      (value, newContext) = (runCode code) context
      newCode = f value
    in
      (runCode newCode) newContext
    }

getContext :: Code Context
getContext =
  Code { runCode = \context -> (context, context) }

setContext :: Context -> Code ()
setContext context = do
  Code { runCode = \_ -> ((), context) }

updateContext :: (Context -> Context) -> Code ()
updateContext f = do
  Code { runCode = \context -> ((), f context) }

getFromContext :: (Context -> t) -> Code t
getFromContext f = do
  context <- getContext
  return $ f context

addToOutput :: String -> Code ()
addToOutput string = do
  context <- getContext
  setContext context {
    output = string : (output context)
  }

addManyToOutput :: [String] -> Code ()
addManyToOutput strings = do
  context <- getContext
  setContext context {
    output = List.foldl' (\acc i -> i : acc) (output context) strings
  }

compileCode :: [String] -> [String] -> [String]
compileCode lines initialOutput =
  output $ snd $ runCode
    (compileCodeLines lines)
    (initialContext initialOutput)

compileCodeLines :: [String] -> Code ()
compileCodeLines [] = return ()
compileCodeLines (line : tail) = do
  lineNumber <- newLine
  addToOutput "<li><pre>"
  addToOutput line
  addToOutput "</pre></li>"
  case tail of
    [] -> return ()
    _ -> do
      compileCodeLines tail

newLine :: Code Int
newLine = do
  context <- getContext
  let newLineNumber = (lineNumber context) + 1
  setContext $ context { lineNumber = newLineNumber }
  return newLineNumber

