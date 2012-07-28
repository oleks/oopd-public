module Html(
  htmlHeader,
  htmlFooter,
  getTeXSpecialHtml
) where

import qualified Control.Monad
import qualified Data.List as List
import qualified Data.Time as Time
import Data.Time.Format(formatTime)
import System.Locale(defaultTimeLocale)


import Grammar

newtype ReverseState state value =
  ReverseState {
    runReverseState :: state -> (value, state)
  }

evalReverseState state function =
  fst (runReverseState function state)

instance Monad (ReverseState state) where
    return value = ReverseState $ (,) value
    ReverseState runner >>= function =
      ReverseState $ \s ->
        let (a,s'') = runner s'
            (b,s') = runReverseState (function a) s
        in (b,s'')

get =
  ReverseState $ \state -> (state, state)
modify function =
  ReverseState $ \state -> ((), function state)
put = modify . const

join :: [String] -> String
join strings = evalReverseState "" $ do
    text <- get
    mapM (\string -> modify (string++)) strings
    return text

htmlHeader =
  join [
    "<!DOCTYPE html>",
    "<html lang='en'>",
    "<head>",
    "<link rel='stylesheet' href='style.css'>",
    "<meta charset='utf-8'>",
    "<title>Fancy Title</title>",
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
    formatTime defaultTimeLocale "%Y %b. %d" utcTime,
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

getTeXSpecialHtml :: TeXSpecial -> String
getTeXSpecialHtml TeXEmDash = "&mdash;"
getTeXSpecialHtml TeXEnDash = "&ndash;"
getTeXSpecialHtml TeXOpenSingleQuote = "&#8216;"
getTeXSpecialHtml TeXCloseSingleQuote = "&#8217;"
getTeXSpecialHtml TeXOpenDoubleQuote = "&#8220;"
getTeXSpecialHtml TeXCloseDoubleQuote = "&#8221;"

