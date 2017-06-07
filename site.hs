--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}
import Data.Monoid ((<>))
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        let ctx = getContext [] ""
        compile $ do
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = getContext posts "Archives"

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match (fromList ["about.md", "contact.md"]) $ do
      compile $ let ctx = getContext [] "" in
        pandocCompiler
        >>= loadAndApplyTemplate "templates/plain.html" ctx
        >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = getContext posts "Home"

            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
getContext :: [Item String] -> String -> Context String
getContext posts title =
  metadataField                                                <>
  listField "posts" ctx (return posts)                         <>
  ctx
  where
    ctx =
      metadataField                                                <>
      constField "title" title                                     <>
      field "about" (\_ -> loadBody (fromFilePath "about.md"))     <>
      field "contact" (\_ -> loadBody (fromFilePath "contact.md")) <>
      defaultContext
