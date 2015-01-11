-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad         (liftM)
import           Data.Monoid           ((<>))
import           Hakyll
import           Text.Pandoc.Options
import           Text.Pandoc.Templates (getDefaultTemplate)
-------------------------------------------------------------------------------

s5DefaultTemplate :: IO String
s5DefaultTemplate = liftM (either (const "") id) . getDefaultTemplate Nothing $ "s5"


getFeedConf :: String -> FeedConfiguration
getFeedConf title = FeedConfiguration
  { feedTitle = "kusut :: " ++ title
  , feedDescription = "kusut"
  , feedAuthorName = "kusut"
  , feedAuthorEmail = "tinokusut@gmail.com"
  , feedRoot = "http://kusut.web.id"
  }


getS5option :: String -> WriterOptions
getS5option template = defaultHakyllWriterOptions
  { writerSlideVariant = S5Slides
  , writerStandalone = True
  , writerVariables = [ ("s5-url", "/static/s5")
                      , ("css", "/static/css/default.css")
                      , ("author-meta", "kusut")
                      , ("highlighting-css", "")
                      ]
  , writerTemplate = template
  }


templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler


css :: Rules ()
css = match "src/Css.hs" $ do
  route $ gsubRoute "src/Css" (const "static/css/default") `composeRoutes` setExtension "css"
  compile $ getResourceString
    >>= withItemBody (unixFilter "cabal" ["exec", "runghc"])


posts :: Rules ()
posts = match "posts/*" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= saveSnapshot "writings"
    >>= loadAndApplyTemplate "templates/post.html" defaultContext
    >>= loadAndApplyTemplate "templates/base.html" defaultContext
    >>= relativizeUrls


slides :: WriterOptions -> Rules ()
slides s5option = match "slides/*" $ do
  route $ setExtension "html"
  compile $ pandocCompilerWith defaultHakyllReaderOptions s5option
    >>= relativizeUrls


s5files :: Rules ()
s5files = do
  match ("slides/s5/*" .&&. complement "slides/s5/pretty.css") $ do
    route $ gsubRoute "slides/s5" (const "static/s5")
    compile $ copyFileCompiler

  match "static/s5/**" $ do
    route   idRoute
    compile copyFileCompiler


feed :: Rules ()
feed = create ["atom.xml"] $ do
  route idRoute
  compile $ do
    let conf = getFeedConf "All posts"
    let context = defaultContext <> bodyField "description"
    posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "writings"
    renderAtom conf context posts


index :: Rules ()
index = match "index.html" $ do
  route idRoute
  compile $ do
    posts <- recentFirst =<< loadAll "posts/*"
    let context = listField "posts" defaultContext (return posts)
                  <> constField "title" "Home"
                  <> defaultContext

    getResourceBody
      >>= applyAsTemplate context
      >>= loadAndApplyTemplate "templates/base.html" context
      >>= relativizeUrls


main :: IO ()
main = do

  s5template <- s5DefaultTemplate
  let s5option = getS5option s5template

  hakyll $ do
    css
    templates
    posts
    slides s5option
    s5files
    feed
    index
