{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S

import Data.Monoid (mappend)
import Data.Maybe (fromJust)
import System.FilePath.Posix
import Text.Pandoc.Options

import Hakyll

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler
  
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler
  

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ mathsCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "js/*.html" $ do
    -- js/x.html -> x/index.html
    route $ (metadataRoute $ \m -> let dir = fromJust $ lookupString "dir" m
                                   in composeRoutes
                                      (gsubRoute dir (const "index"))
                                      (gsubRoute "js/" (const . (++ [pathSeparator]) $ dir)))
  
    compile getResourceBody

  match "js/css/*.css" $ do
    -- js/css/y.css -> y/styles.css
    route $ customRoute $ \id -> let fp = toFilePath id
                                 in "." </> takeBaseName fp </> "styles.css"

    compile copyFileCompiler

  match "js/js/*.js" $ do
    route $ (metadataRoute $ \m -> let out = fromJust $ lookupString "out" m
                                   in constRoute ("." </> out))
    compile getResourceBody

  create ["about.html"] $ do
    route idRoute
    compile $ do
      let aboutCtx = constField "title" "About" <> defaultContext
      
      makeItem ""
        >>= loadAndApplyTemplate "templates/about.html" aboutCtx
        >>= loadAndApplyTemplate "templates/default.html" aboutCtx
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
      js <- loadAll "js/*.html"
      
      let indexCtx =
            listField "posts" postCtx (return posts)  <>
            listField "js" defaultContext (return js) <>
            constField "title" "Home"                 <>
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

mathsCompiler =
  let mathsExt = [Ext_tex_math_dollars, Ext_smart]
      defaultExt = writerExtensions defaultHakyllWriterOptions
      newExt = defaultExt <> extensionsFromList mathsExt
      writerOptions = defaultHakyllWriterOptions { writerExtensions = newExt,
                                                   writerHTMLMathMethod = MathJax "" }
  in pandocCompilerWith defaultHakyllReaderOptions writerOptions
