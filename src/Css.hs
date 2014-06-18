{-# LANGUAGE OverloadedStrings #-}
import           Clay
import qualified Data.Text.Lazy.IO as Text
import           Prelude           hiding (div)

header_ :: Css
header_ = body |> div # "#header" ? do
    textAlign (alignSide sideRight)
    borderBottom solid (px 2) "#6f6f6f"
    marginBottom (px 30)
    padding 12 0 12 0

footer_ :: Css
footer_ = body |> div # "#footer" ? do
    textAlign (alignSide sideCenter)
    borderTop solid (px 2) "#6f6f6f"
    marginTop (px 30)
    padding 12 0 12 0

global :: Css
global = do
    a ? color "#9fafaf"
    a # visited ? color "#8f8f8f"

    div # ".slide" |> pre ? do
        sym padding (px 0)
        borderWidth (px 0)

    pre ? do
        sym padding (px 10)
        border solid (px 1) "#6f6f6f"
        overflow auto

    div # ".info" ? do
        color "#8f8f8f"
        fontStyle italic

    body ? do
        backgroundColor "#3f3f3f"
        color "#dcdccc"
        fontSize (px 13)
        maxWidth (px 700)
        marginLeft auto
        marginRight auto

compiled :: Css
compiled = div # ".pandoc" ? do
    h1 ? fontSize (em 1.5)
    h2 ? fontSize (em 1.1)


zenburn :: Css
zenburn = do
    code |> "span.kw" ? do
       color "#f0dfaf"
       fontWeight bold
    code # ".haskell" |> "span.dt" ? color "#7CB8BB"
    code # ".haskell" |> "span.fu" ? color "#DFAF8F"
    code |> "span.dt" ? fontWeight bold
    code |> "span.dv" ? color "#dcdccc"
    code |> "span.bn" ? color "#dca3a3"
    code |> "span.fl" ? color "#c0bed1"
    code |> "span.ch" ? color "#dca3a3"
    code |> "span.st" ? color "#cc9393"
    code |> "span.co" ? color "#7f9f7f"
    code |> "span.ot" ? color "#93e0e3"
    code |> "span.al" ? color "#ffcfaf"
    code |> "span.fu" ? color "#93e0e3"
    code |> "span.er" ? color "#c3bf9f"


main :: IO ()
main = Text.putStr $ renderWith compact $ global >> header_ >> footer_ >> compiled >> zenburn

