module Html(
  htmlHeader,
  htmlFooter,
  getTeXSpecialHtml,
  htmlBegin,
  htmlEnd,
  htmlCounterAnchor
) where

import qualified Data.Time as Time
import Data.Time.Format(formatTime)
import System.Locale(defaultTimeLocale)
import Text.Show(showString)

import Grammar
import String

class HtmlAnchor a where
  idPrefix :: a -> String
  textPrefix :: a -> String

instance HtmlAnchor Counter where

  idPrefix DefinitionCounter = "D"
  idPrefix ListingCounter = "L"
  idPrefix SectionCounter = "S"

  textPrefix DefinitionCounter = "Definition"
  textPrefix ListingCounter = "Listing"
  textPrefix SectionCounter = "&sect;"

htmlHeader :: String -> String
htmlHeader htmlTitle =
  join [
    "<!DOCTYPE html>",
    "<html lang='en'>",
    "<head>",
    "<link rel='stylesheet' href='style.css'>",
    "<meta charset='utf-8'>",
    "<title>", htmlTitle, "</title>",
    "</head>",
    "<body><article>"
  ]

htmlFooter :: Time.UTCTime -> String
htmlFooter utcTime =
  join [
    "</article><footer>",
    "<address>",
    "<div>",
    externalLink "diku" "http://www.diku.dk" "Datalogisk institut",
    "</div>",
    "<div>",
    externalLink "ku" "http://www.ku.dk" "Copenhagen University",
    "</div>",
    "<time datetime='",
    formatTime defaultTimeLocale "%Y-%m-%d" utcTime,
    "'>",
    formatTime defaultTimeLocale "%B %d, %Y" utcTime,
    "</span>",
    "</address>",
    "</footer></body></html>"
  ]

externalLink :: String -> String -> String -> String
externalLink htmlClass href text =
  join [
    "<a class='",
    htmlClass,
    "' href='",
    href,
    "' target='_blank'>",
    text,
    "</a>"
  ]

htmlCounterAnchor :: Counter -> [Int] -> String
htmlCounterAnchor counter numberStack =
  let
    numbers = reverseJoinWith (map show numberStack) "."
    textNumbers =
      case counter of
        SectionCounter ->
          (showString numbers ".&nbsp;")
        _ -> numbers
  in
    htmlCounterAnchorAux counter numbers textNumbers

htmlCounterAnchorAux :: Counter -> String -> String -> String
htmlCounterAnchorAux counter idNumbers textNumbers =
  let
    idText = showString (idPrefix counter) ('.':idNumbers)
    textText = showString (textPrefix counter) (' ':textNumbers)
  in join ["<a class='margin' id='",
    idText,
    "' href='#",
    idText,
    "'>",
    textText,
    "</a>"]

getTeXSpecialHtml :: TeXSpecial -> String
getTeXSpecialHtml TeXEmDash = "&mdash;"
getTeXSpecialHtml TeXEnDash = "&ndash;"
getTeXSpecialHtml TeXOpenSingleQuote = "&#8216;"
getTeXSpecialHtml TeXCloseSingleQuote = "&#8217;"
getTeXSpecialHtml TeXOpenDoubleQuote = "&#8220;"
getTeXSpecialHtml TeXCloseDoubleQuote = "&#8221;"

htmlTagName :: Environment -> String
htmlTagName environment =
  case environment of
    Enumerate -> "ol"
    Itemize -> "ul"
    Definition -> "dfn"

htmlBegin :: Environment -> String
htmlBegin environment =
  showString "<" $ showString (htmlTagName environment) ">"

htmlEnd :: Environment -> String
htmlEnd environment =
  showString "</" $ showString (htmlTagName environment) ">"
