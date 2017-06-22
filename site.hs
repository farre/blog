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
      create [fromCapture "rss/*.xml" tag] $ do
        route $ idRoute
        compile $ do
          posts <- recentFirst =<< loadAllSnapshots p "rss"
          let ctx = getContext tags' posts Nothing <> bodyField "description"
          renderRss (feedConfiguration (tag ++ " - ") tag) ctx posts

      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll p
        let ctx = getContext Nothing posts (Just $ "Tag " ++ show tag) <>
                  constField "tag" (tag)
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        let ctx = constField "post" "" <> getContext tags' [] Nothing
            rssCtx = teaserField "teaser" "content" <> ctx
        compile $ do
          res <- pandocCompiler
          saveSnapshot "content" res
            >>= loadAndApplyTemplate "templates/rssitem.html" rssCtx
            >>= saveSnapshot "rss"
          loadAndApplyTemplate "templates/post.html" ctx res
            >>= loadAndApplyTemplate "templates/comments.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = getContext tags' posts (Just "Archive")
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match (fromList ["about.md", "contact.md"]) $ do
      compile $ let ctx = getContext tags' [] Nothing in
        pandocCompiler
        >>= loadAndApplyTemplate "templates/plain.html" ctx
        >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let ctx = getContext tags' posts Nothing

            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots "posts/*" "rss"
        let ctx = getContext tags' posts Nothing <> bodyField "description"
        renderRss (feedConfiguration "" "all posts") ctx posts

--------------------------------------------------------------------------------
getContext :: Maybe Tags -> [Item String] -> Maybe String -> Context String
getContext tags posts title =
  listField "posts" (teaserField "teaser" "content" <> ctx) (return posts) <>
  maybe mempty (constField "title") title                                  <>
  ctx
  where
    ctx =
      field "about" (\_ -> loadBody (fromFilePath "about.md"))       <>
      field "contact" (\_ -> loadBody (fromFilePath "contact.md"))   <>
      maybe mempty (tagsField "tags") tags                           <>
      metadataField                                                  <>
      defaultContext

feedConfiguration title tag = FeedConfiguration
  { feedTitle = title ++ "farre's blog"
  , feedDescription = "Feed for " ++ tag
  , feedAuthorName = "Andreas farre"
  , feedAuthorEmail = "farre@mozilla.com"
  , feedRoot = "https://farre.github.io/blog"
  }
