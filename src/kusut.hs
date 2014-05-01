-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.Monoid            ((<>))
import           Hakyll
import           Text.Highlighting.Kate (styleToCss, zenburn)
-------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    match "static/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    create ["static/css/zenburn.css"] $ do
        route   idRoute
        compile $ makeItem (compressCss . styleToCss $ zenburn)

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    match "index.html" $ do
         route idRoute
         compile $ do
             posts <- recentFirst =<< loadAll "posts/*"
             let indexContext =
                    listField "posts" defaultContext (return posts)
                    <> constField "title" "Home"
                    <> defaultContext

             getResourceBody
                 >>= applyAsTemplate indexContext
                 >>= loadAndApplyTemplate "templates/base.html" indexContext
                 >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
