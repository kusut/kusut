{-# LANGUAGE OverloadedStrings #-}
import           Clay
import qualified Data.Text.Lazy.IO as Text
import           Prelude           hiding (div)

header_ :: Css
header_ = div # "#header" ? do
    textAlign (alignSide sideRight)
    borderBottom solid (px 2) "#6f6f6f"
    marginBottom (px 30)
    padding 12 0 12 0

footer_ :: Css
footer_ = div # "#footer" ? do
    textAlign (alignSide sideCenter)
    borderTop solid (px 2) "#6f6f6f"
    marginTop (px 30)
    padding 12 0 12 0

global :: Css
global = do
    a ? color "#9fafaf"
    a # visited ? color "#8f8f8f"

    pre ? do
        sym padding (px 10)
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

main :: IO ()
main = Text.putStr $ renderWith compact $ global >> header_ >> footer_ >> compiled

