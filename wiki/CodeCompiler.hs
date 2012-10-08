module CodeCompiler(compileCode) where

data Context
  = Context {
    output :: [String],
    lineNumber :: Int
  }

initialContext :: [String] -> Context
initialContext output'
  = Context {
    output = output',
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

addToOutput :: String -> Code ()
addToOutput string = do
  context <- getContext
  setContext context {
    output = string : (output context)
  }

compileCode :: [String] -> [String] -> [String]
compileCode codeLines initialOutput =
  output $ snd $ runCode
    (mapM_ compileCodeLine codeLines)
    (initialContext initialOutput)

compileCodeLine :: String -> Code ()
compileCodeLine codeLine = do
  addToOutput "<li><pre>"
  addToOutput codeLine
  addToOutput "</pre></li>"

