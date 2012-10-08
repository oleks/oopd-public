module Compiler(compile) where

import Data.List(foldl')
import Data.Time(UTCTime)

import Grammar
import CodeCompiler
import Html

data Context
  = Context {
    environmentStack :: [Environment],
    paragraphLength :: Int,
    output :: [String],
    contextParagraph :: String,
    contextTeXemes :: [TeXeme],
    contextItem :: Bool,
    groupStack :: [String],
    headerDepth :: Int,
    headerStack :: [Int],
    contextSuppressSpace :: Bool,
    listingCounter :: Int,
    definitionCounter :: Int,
    title :: String
  }

initialContext :: Context
initialContext
  = Context {
    environmentStack = [],
    paragraphLength = 0,
    output = [],
    contextParagraph = "",
    contextTeXemes = [],
    contextItem = False,
    groupStack = [],
    headerDepth = 0,
    headerStack = [],
    contextSuppressSpace = False,
    listingCounter = 0,
    definitionCounter = 0,
    title = "Untitled"
  }

data TeX t = TeX {
  runTeX :: Context -> (t, Context)
}

instance Monad TeX where
  return value = TeX { runTeX = \context -> (value, context) }
  tex >>= f = TeX { runTeX = \context ->
    let
      (value, newContext) = (runTeX tex) context
      newTeX = f value
    in
      (runTeX newTeX) newContext
    }

getContext :: TeX Context
getContext =
  TeX { runTeX = \context -> (context, context) }

setContext :: Context -> TeX ()
setContext context = do
  TeX { runTeX = \_ -> ((), context) }

modify :: (Context -> Context) -> TeX ()
modify f = do
  TeX { runTeX = \context -> ((), f context) }

compile :: UTCTime -> LaTeX -> String
compile utcTime latex =
  let
    (_, context) = (runTeX (compileLaTeX latex)) initialContext
    header = htmlHeader (title context)
    footer = htmlFooter utcTime
  in
    showString header $
    showString (foldl' (\text i -> showString i text) "" (output context)) $
    footer

compileLaTeX :: LaTeX -> TeX ()
compileLaTeX (Paragraphs paragraphs) = do
  mapM_ compileParagraph paragraphs
  return ()

openParagraph :: [TeXeme] -> TeX ()
openParagraph texemes = do
  context <- getContext
  if inParagraph context
  then fail "Paragraph is already open."
  else setContext context {
    contextSuppressSpace = True,
    contextTeXemes = texemes
  }

closeParagraph :: TeX ()
closeParagraph = do
  context <- getContext
  if inParagraph context
  then do
    addManyToOutput ["<p>", (contextParagraph context), "</p>"]
    resetParagraphContext
  else return ()

simpleCloseParagraph :: TeX ()
simpleCloseParagraph = do
  context <- getContext
  if inParagraph context
  then do
    addToOutput (contextParagraph context)
    resetParagraphContext
  else return ()

resetParagraphContext :: TeX ()
resetParagraphContext = do
  context <- getContext
  setContext $ context {
    paragraphLength = 0,
    contextParagraph = "",
    contextTeXemes = []
  }

addToParagraph :: String -> TeX ()
addToParagraph string = do
  context <- getContext
  setContext context {
    paragraphLength = (paragraphLength context) + (length string),
    contextParagraph =
      showString (contextParagraph context) string
  }

inParagraph :: Context -> Bool
inParagraph context = paragraphLength context > 0

openListItem :: TeX ()
openListItem = do
  closeParagraph
  context <- getContext
  if contextItem context
  then fail "Item is already open."
  else do
    addToOutput "<li>"
    context2 <- getContext
    setContext context2 {
      contextItem = True
    }

closeListItem :: TeX ()
closeListItem = do
  closeParagraph
  context <- getContext
  if contextItem context
  then do
    addToOutput "</li>"
    context2 <- getContext
    setContext context2 {
      contextItem = False
    }
  else return ()

openCodeLine :: TeX ()
openCodeLine = do
  simpleCloseParagraph
  context <- getContext
  if contextItem context
  then fail "Item is already open."
  else do
    addToOutput "<li><pre>"
    context2 <- getContext
    setContext context2 {
      contextItem = True
    }

closeCodeLine :: TeX ()
closeCodeLine = do
  simpleCloseParagraph
  context <- getContext
  if contextItem context
  then do
    addToOutput "</pre></li>"
    context2 <- getContext
    setContext context2 {
      contextItem = False
    }
  else return ()

addToGroupStack :: String -> TeX ()
addToGroupStack name = do
  context <- getContext
  setContext context {
    groupStack = name : (groupStack context)
  }
  addToParagraph $ showString "<" $ showString name ">"

compileParagraph :: Paragraph -> TeX ()
compileParagraph (TeXemes texemes) = do

  openParagraph texemes

  compileTeXemes texemes

  context <- getContext
  if inParagraph context
  then closeParagraph
  else return ()

suppressSpace :: TeX ()
suppressSpace = modify (\context -> context {
    contextSuppressSpace = True
  })

compileTeXemes :: [TeXeme] -> TeX ()
compileTeXemes [] = return ()
compileTeXemes ((TeXRaw text):ts) = do
  case (length text) of
    0 -> return ()
    _ -> do
      addToParagraph text
  compileTeXemes ts
compileTeXemes ((TeXVerbatim text):ts) = do
  closeParagraph
  addManyToOutput ["<pre>", text, "</pre>"]
  compileTeXemes ts
compileTeXemes ((TeXBegin environment):ts) = do
  closeParagraph
  writeBegin environment
  modify (\ context -> context {
    environmentStack = environment : (environmentStack context)
  })
  compileTeXemes ts
compileTeXemes ((TeXEnd environment):ts) = do
  closeListItem
  context <- getContext
  case (environmentStack context) of
    (frame : frames) ->
      if frame == environment
      then do
        addToOutput $ htmlEnd environment
        modify (\ context2 ->
          context2 { environmentStack = frames })
      else fail "stack underflow 0"
    _ -> fail "stack underflow 1"
  compileTeXemes ts
compileTeXemes (
  (TeXCommand "assignment") :
  (TeXGroup ttl) :
  ts) = do
    closeParagraph
    advanceHeader 2
    addToOutput "<h2>"
    writeSectionAnchor
    compileTeXGroup ttl
    simpleCloseParagraph
    addToOutput "</h2>"
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "chapter") :
  (TeXGroup ttl) :
  ts) = do
    closeParagraph
    advanceHeader 1
    addToOutput "<h1>"
    writeSectionAnchor
    compileTeXGroup ttl

    context <- getContext
    setContext context { title = contextParagraph context }

    simpleCloseParagraph
    addToOutput "</h1>"
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "item") :
  ts) = do
    closeListItem
    openListItem
    compileTeXemes ts
compileTeXemes (
  [TeXCommand "ldots"]) = do
    addToParagraph "&hellip;"
    closeParagraph
compileTeXemes (
  (TeXCommand "ldots") :
  ts) = do
    addToParagraph "&hellip;"
    compileTeXemes ts
compileTeXemes (
  TeXSpecial texSpecial :
  ts) = do
    addToParagraph $ getTeXSpecialHtml texSpecial
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "bf") :
  ts) = do
    addToGroupStack "b"
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "it") :
  ts) = do
    addToGroupStack "i"
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "emph") :
  (TeXGroup paragraphs) :
  ts) = do
    addToParagraph "<em>"
    suppressSpace
    compileTeXGroup paragraphs
    addToParagraph "</em>"
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "var") :
  (TeXGroup paragraphs) :
  ts) = do
    addToParagraph "<var>"
    compileTeXGroup paragraphs
    addToParagraph "</var>"
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "code") :
  (TeXGroup paragraphs) :
  ts) = do
    addToParagraph "<code>"
    compileTeXGroup paragraphs
    addToParagraph "</code>"
    compileTeXemes ts
compileTeXemes (
  (TeXCodeBox codebox) :
  ts) = do
    closeParagraph
    writeListingAnchor
    addToOutput "<code class='listing'><ol>"
    mapM_ compileCodeBoxElement codebox
    closeCodeLine
    simpleCloseParagraph
    addToOutput "</ol></code>"
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "mono") :
  (TeXGroup paragraphs) :
  ts) = do
    addToParagraph "<tt>"
    compileTeXGroup paragraphs
    addToParagraph "</tt>"
    compileTeXemes ts
compileTeXemes (
  (TeXCode code) :
  ts) = do
    closeParagraph
    writeListingAnchor
    addToOutput "<code class='listing'><ol>"
    liftOutput (compileCode code)
    addToOutput "</ol></code>"
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "wikipedia") :
  (TeXGroup page) :
  (TeXGroup text) :
  ts) = do
    addToParagraph "<a target='_blank' href='http://en.wikipedia.org/wiki/"
    suppressSpace
    compileTeXGroup page
    addToParagraph "'>"
    suppressSpace
    compileTeXGroup text
    addToParagraph "</a>"
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "link") :
  (TeXGroup text) :
  (TeXGroup link) :
  ts) = do
    addToParagraph "<a target='_blank' href='"
    suppressSpace
    compileTeXGroup link
    addToParagraph "'>"
    suppressSpace
    compileTeXGroup text
    addToParagraph "</a>"
    compileTeXemes ts
compileTeXemes (
  (TeXCommand "explain") :
  (TeXGroup text) :
  (TeXGroup explanation) :
  ts) = do
    addToParagraph "<span class='tooltip' title='"
    suppressSpace
    compileTeXGroup explanation
    addToParagraph "'>"
    suppressSpace
    compileTeXGroup text
    addToParagraph "</span>"
    compileTeXemes ts
compileTeXemes (
  (TeXGroup paragraphs) :
  ts) = do
    compileTeXGroup paragraphs
    compileTeXemes ts
compileTeXemes (
  (TeXComment _) :
  ts) = do
    compileTeXemes ts
compileTeXemes _ = fail "noob"

compileTeXGroup :: [Paragraph] -> TeX ()
compileTeXGroup paragraphs = do
  mapM_ (\(TeXemes texemes) -> compileTeXemesAux texemes) paragraphs

  context <- getContext
  mapM_ closeGroupAux (groupStack context)
  context2 <- getContext
  setContext context2 {
    groupStack = []
  }

compileTeXemesAux :: [TeXeme] -> TeX ()
compileTeXemesAux texemes = do
  context <- getContext
  oldTeXemes <- return $ contextTeXemes context
  setContext context {
    contextSuppressSpace = True,
    contextTeXemes = texemes
  }

  compileTeXemes texemes

  context2 <- getContext
  setContext context2 {
    contextTeXemes = oldTeXemes
  }

addToOutput :: String -> TeX ()
addToOutput string = do
  context <- getContext
  setContext context {
    output = string : (output context)
  }

addManyToOutput :: [String] -> TeX ()
addManyToOutput strings = do
  context <- getContext
  setContext context {
    output = foldl' (\acc i -> i : acc) (output context) strings
  }

liftOutput :: ([String] -> [String]) -> TeX ()
liftOutput function = do
  context <- getContext
  setContext $ context {
    output = function (output context)
  }

closeGroupAux :: String -> TeX ()
closeGroupAux name = do
  addToParagraph $ showString "</" $ showString name ">"

fixStack :: Int -> [Int] -> [Int]
fixStack 0 stack = stack

advanceHeader :: Int -> TeX ()
advanceHeader depth = do
  context <- getContext
  if headerDepth context < 1
  then setContext context {
    headerDepth = (headerDepth context) + 1,
    headerStack = 0 : (headerStack context)
  }
  else
    case (headerStack context) of
      [] -> fail "header stack underflow"
      (currentNumber : numbers) -> setContext context {
        headerStack = (currentNumber + 1) : numbers
      }

writeSectionAnchor :: TeX ()
writeSectionAnchor = do
  context <- getContext
  addToOutput $ htmlCounterAnchor SectionCounter (headerStack context)

writeListingAnchor :: TeX ()
writeListingAnchor = do
  context <- getContext
  let listing = (listingCounter context)
  let numbers = (listing : (headerStack context))
  setContext $ context { listingCounter = listing + 1 }
  addToOutput $ htmlCounterAnchor ListingCounter numbers

writeDefinitionAnchor :: TeX ()
writeDefinitionAnchor = do
  context <- getContext
  let definition = (definitionCounter context)
  let numbers = (definition : (headerStack context))
  setContext $ context { definitionCounter = definition + 1 }
  addToOutput $ htmlCounterAnchor DefinitionCounter numbers

{-- TODO: The writeXXXAnchor interfaces are not consistent. writeSectionAnchor
does not advance the header whereas the rest do. --}

compileCodeBoxElement :: TeXCode -> TeX ()
compileCodeBoxElement TeXCodeLt = do
  addToParagraph "&lt;"
compileCodeBoxElement TeXCodeGt = do
  addToParagraph "&gt;"
compileCodeBoxElement TeXCodeLi = do
  closeCodeLine
  openCodeLine
compileCodeBoxElement TeXCodeZi = do
  context <- getContext
  if inParagraph context
  then addToParagraph "</br>"
  else return ()
compileCodeBoxElement (TeXCodeRaw string) = do
  addToParagraph string

writeBegin :: Environment -> TeX ()
writeBegin environment = do
  case environment of
    Definition -> writeDefinitionAnchor
    _ -> return ()
  addToOutput $ htmlBegin environment

