{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler
  
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler
  
  match (fromList ["about.rst", "contact.markdown", "about.html"]) $ do
    route   $ setExtension "html"
    compile $ do
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts) <>
            constField "title" "Archives"            <>
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- take 3 <$> (recentFirst =<< loadAll "posts/*")
      let indexCtx =
            listField "posts" postCtx (return posts) <>
            constField "title" "Home"                <>
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext