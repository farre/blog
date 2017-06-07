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

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let tags' = Just tags

    tagsRules tags $ \tag p -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll p
            let ctx = getContext Nothing posts
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        let ctx = getContext tags' []
        compile $ do
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = getContext tags' posts
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match (fromList ["about.md", "contact.md"]) $ do
      compile $ let ctx = getContext tags' [] in
        pandocCompiler
        >>= loadAndApplyTemplate "templates/plain.html" ctx
        >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = getContext tags' posts

            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
getContext :: Maybe Tags -> [Item String] -> Context String
getContext tags posts =
  listField "posts" ctx (return posts) <>
  maybe mempty (tagsField "tags") tags <>
  metadataField                        <>
  ctx
  where
    ctx =
      field "about" (\_ -> loadBody (fromFilePath "about.md"))     <>
      field "contact" (\_ -> loadBody (fromFilePath "contact.md")) <>
      defaultContext
