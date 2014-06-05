-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad          (liftM)
import           Data.Monoid            ((<>))
import           Hakyll
import           Text.Highlighting.Kate (styleToCss, zenburn)
import           Text.Pandoc.Options
import           Text.Pandoc.Templates  (getDefaultTemplate)
-------------------------------------------------------------------------------

s5DefaultTemplate :: IO String
s5DefaultTemplate  = liftM (either (const "") id) . getDefaultTemplate Nothing $ "s5"

main :: IO ()
main = do

    template <- s5DefaultTemplate
    let s5Options = defaultHakyllWriterOptions
                      { writerSlideVariant = S5Slides
                      , writerHighlight = True
                      , writerHighlightStyle = zenburn
                      , writerStandalone = True
                      , writerVariables = [("s5-url", "/static/s5"),("author-meta", "kusut")]
                      , writerTemplate = template
                      }

    hakyll $ do

        match "static/s5/**" $ do
            route   idRoute
            compile copyFileCompiler

        create ["static/css/zenburn.css"] $ do
            route   idRoute
            compile $ makeItem (compressCss . styleToCss $ zenburn)

        match "src/Css.hs" $ do
            route $ gsubRoute "src/Css" (const "static/css/default") `composeRoutes` setExtension "css"
            compile $ getResourceString >>= withItemBody (unixFilter "cabal" ["exec", "runghc"])

        match "posts/*" $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= saveSnapshot "writings"
                >>= loadAndApplyTemplate "templates/post.html" defaultContext
                >>= loadAndApplyTemplate "templates/base.html" defaultContext
                >>= relativizeUrls

        match "slides/*" $ do
            route $ setExtension "html"
            compile $ pandocCompilerWith defaultHakyllReaderOptions s5Options >>= relativizeUrls

        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/*"
                let indexContext = listField "posts" defaultContext (return posts)
                                 <> constField "title" "Home"
                                 <> defaultContext

                getResourceBody
                    >>= applyAsTemplate indexContext
                    >>= loadAndApplyTemplate "templates/base.html" indexContext
                    >>= relativizeUrls

        create ["atom.xml"] $ do
            route idRoute
            compile $ do
                loadAllSnapshots "posts/*" "writings"
                    >>= fmap (take 10) . recentFirst
                    >>= renderAtom (feedConfiguration "All posts") ( defaultContext <> bodyField "description")

        match "templates/*" $ compile templateCompiler


feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle = "kusut :: " ++ title
    , feedDescription = "kusut"
    , feedAuthorName = "kusut"
    , feedAuthorEmail = "tinokusut@gmail.com"
    , feedRoot = "http://kusut.web.id"
    }

