module Compiler(compile) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable
import qualified Data.Time as Time
import Text.Show

import Grammar
import CodeCompiler
import Html

data State = OrderedList | OrderedItem

type Stack = [State]

data Context
  = Context {
    environmentStack :: [String],
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

environmentMap :: Map.Map String (String, String)
environmentMap = Map.fromList [
  ("enumerate", ("<ol>", "</ol>")),
  ("itemize", ("<ul>", "</ul>")),
  ("definition", ("<dfn>", "</dfn>"))]

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

updateContext :: (Context -> Context) -> TeX ()
updateContext f = do
  TeX { runTeX = \context -> ((), f context) }

getFromContext :: (Context -> t) -> TeX t
getFromContext f = do
  context <- getContext
  return $ f context

compile :: Time.UTCTime -> LaTeX -> String
compile utcTime latex =
  let
    (_, context) = (runTeX (compileLaTeX latex)) initialContext
    header = htmlHeader (title context)
    footer = htmlFooter utcTime
  in
    showString header $
    showString (List.foldl' (\text i -> showString i text) "" (output context)) $
    footer

compileLaTeX :: LaTeX -> TeX ()
compileLaTeX (Paragraphs paragraphs) = do
  mapM compileParagraph paragraphs
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
    context <- getContext
    setContext context {
      contextItem = True
    }

closeListItem :: TeX ()
closeListItem = do
  closeParagraph
  context <- getContext
  if contextItem context
  then do
    addToOutput "</li>"
    context <- getContext
    setContext context {
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
    context <- getContext
    setContext context {
      contextItem = True
    }

closeCodeLine :: TeX ()
closeCodeLine = do
  simpleCloseParagraph
  context <- getContext
  if contextItem context
  then do
    addToOutput "</pre></li>"
    context <- getContext
    setContext context {
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
suppressSpace = updateContext (\context -> context {
    contextSuppressSpace = True
  })

unsuppressSpace :: TeX ()
unsuppressSpace = updateContext (\context -> context {
    contextSuppressSpace = False
  })

compileTeXemes :: [TeXeme] -> TeX ()
compileTeXemes [] = return ()
compileTeXemes ((TeXRaw text):tail) = do
  case (List.length text) of
    0 -> return ()
    _ -> do
      addToParagraph text
  compileTeXemes tail
compileTeXemes ((TeXVerbatim text):tail) = do
  closeParagraph
  addManyToOutput ["<pre>", text, "</pre>"]
  compileTeXemes tail
compileTeXemes ((TeXBegin name):tail) = do
  closeParagraph
  case name of
    "definition" -> writeDefinitionCounter
    _ -> return ()
  addToOutput $ fst $ environmentMap Map.! name
  context <- getContext
  setContext context {
    environmentStack = name : (environmentStack context)
  }
  compileTeXemes tail
compileTeXemes ((TeXEnd name):tail) = do
  closeListItem
  context <- getContext
  case (environmentStack context) of
    (frame : tail) ->
      if frame == name
      then do
        addToOutput $ snd (environmentMap Map.! name)
        context <- getContext
        setContext context {
          environmentStack = tail
        }
      else fail "stack underflow 0"
    _ -> fail "stack underflow 1"
  context <- getContext
  compileTeXemes tail
compileTeXemes (
  (TeXCommand "chapter") :
  (TeXGroup title) :
  tail) = do
    closeParagraph
    advanceHeader 1
    numeral <- getHeaderNumeral
    addManyToOutput ["<h1>", numeral]
    compileTeXGroup title

    context <- getContext
    setContext context { title = contextParagraph context }

    simpleCloseParagraph
    addToOutput "</h1>"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "item") :
  tail) = do
    closeListItem
    openListItem
    compileTeXemes tail
compileTeXemes (
  [TeXCommand "ldots"]) = do
    addToParagraph "&hellip;"
    closeParagraph
compileTeXemes (
  (TeXCommand "ldots") :
  tail) = do
    addToParagraph "&hellip;"
    compileTeXemes tail
compileTeXemes (
  TeXSpecial texSpecial :
  tail) = do
    addToParagraph $ getTeXSpecialHtml texSpecial
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "bf") :
  tail) = do
    addToGroupStack "b"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "it") :
  tail) = do
    addToGroupStack "i"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "emph") :
  (TeXGroup paragraphs) :
  tail) = do
    addToParagraph "<em>"
    suppressSpace
    compileTeXGroup paragraphs
    addToParagraph "</em>"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "var") :
  (TeXGroup paragraphs) :
  tail) = do
    addToParagraph "<var>"
    compileTeXGroup paragraphs
    addToParagraph "</var>"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "code") :
  (TeXGroup paragraphs) :
  tail) = do
    addToParagraph "<code>"
    compileTeXGroup paragraphs
    addToParagraph "</code>"
    compileTeXemes tail
compileTeXemes (
  (TeXCodeBox codebox) :
  tail) = do
    closeParagraph
    addListingAnchor
    addToOutput "<code class='listing'><ol>"
    mapM compileCodeBoxElement codebox
    closeCodeLine
    simpleCloseParagraph
    addToOutput "</ol></code>"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "mono") :
  (TeXGroup paragraphs) :
  tail) = do
    addToParagraph "<tt>"
    compileTeXGroup paragraphs
    addToParagraph "</tt>"
    compileTeXemes tail
compileTeXemes (
  (TeXCode code) :
  tail) = do
    closeParagraph
    addListingAnchor
    addToOutput "<code class='listing'><ol>"
    liftOutput (compileCode code)
    addToOutput "</ol></code>"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "wikipedia") :
  (TeXGroup page) :
  (TeXGroup text) :
  tail) = do
    addToParagraph "<a target='_blank' href='http://en.wikipedia.org/wiki/"
    suppressSpace
    compileTeXGroup page
    addToParagraph "'>"
    suppressSpace
    compileTeXGroup text
    addToParagraph "</a>"
    compileTeXemes tail
compileTeXemes (
  (TeXCommand "explain") :
  (TeXGroup text) :
  (TeXGroup explanation) :
  tail) = do
    addToParagraph "<span class='tooltip' title='"
    suppressSpace
    compileTeXGroup explanation
    addToParagraph "'>"
    suppressSpace
    compileTeXGroup text
    addToParagraph "</span>"
    compileTeXemes tail
compileTeXemes (
  (TeXGroup paragraphs) :
  tail) = do
    compileTeXGroup paragraphs
    compileTeXemes tail

addListingAnchor :: TeX ()
addListingAnchor = do
  context <- getContext
  let listing = (listingCounter context)
  let numbers = (joinNumbers (headerStack context)) ++ ('.' : (show listing))
  setContext $ context { listingCounter = listing + 1 }
  addManyToOutput ["<a class='margin' id='L.",
    numbers,
    "' href='#L.",
    numbers,
    "'>Listing ",
    numbers,
    "</a>"]

addParagraphSpace :: TeX ()
addParagraphSpace = do
  length <- getParagraphLength
  if length > 0
  then addToParagraph " "
  else return ()

tryAddParagraphSpace :: TeX ()
tryAddParagraphSpace = do
  context <- getContext
  if contextSuppressSpace context
  then return ()
  else addParagraphSpace

compileTeXGroup :: [Paragraph] -> TeX ()
compileTeXGroup paragraphs = do
  mapM (\(TeXemes texemes) -> compileTeXemesAux texemes) paragraphs

  context <- getContext
  mapM closeGroupAux (groupStack context)
  context <- getContext
  setContext context {
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

  context <- getContext
  setContext context {
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
    output = List.foldl' (\acc i -> i : acc) (output context) strings
  }

liftOutput :: ([String] -> [String]) -> TeX ()
liftOutput function = do
  context <- getContext
  setContext $ context {
    output = function (output context)
  }

getParagraphLength :: TeX Int
getParagraphLength = do
  context <- getContext
  return $ paragraphLength context

closeGroupAux :: String -> TeX ()
closeGroupAux name = do
  addToParagraph $ showString "</" $ showString name ">"

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
      (currentNumber:tail) -> setContext context {
        headerStack = (currentNumber + 1) : tail
      }

getHeaderNumeral :: TeX String
getHeaderNumeral = do
  context <- getContext
  let numbers = joinNumbers (headerStack context)
  return $
    showString "<a class='margin' id='S." $
    showString numbers $
    showString "'>&sect; " $
    showString numbers ".&nbsp;</a>"

joinNumbers :: [Int] -> String
joinNumbers [] = ""
joinNumbers (number : tail) =
  List.foldl' (\text i -> showString (show i) text) (show number) tail


writeDefinitionCounter :: TeX ()
writeDefinitionCounter = do
  context <- getContext
  let definition = (definitionCounter context)
  let numbers = (joinNumbers (headerStack context)) ++ ('.' : (show definition))
  setContext $ context { definitionCounter = definition + 1 }
  addManyToOutput ["<a class='margin' id='D.",
    numbers,
    "' href='#D.",
    numbers,
    "'>Definition ",
    numbers,
    "</a>"]


compileCodeBoxElement :: TeXCode -> TeX ()
compileCodeBoxElement TeXCodeLi = do
  closeCodeLine
  openCodeLine
compileCodeBoxElement TeXCodeZi = do
  addToParagraph "\n"
compileCodeBoxElement (TeXCodeRaw string) = do
  addToParagraph string
