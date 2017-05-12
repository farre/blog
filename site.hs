--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}
import Data.List (sortBy)
import Data.Maybe
import Data.Monoid (mappend)
import Data.Ord
import Data.Time.Clock
import Data.Time.Format
import Control.Monad
import Hakyll

import Debug.Trace
import Text.Pandoc.Definition
import Text.Pandoc.Options

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
        compile $ do
          myPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- myRecentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    field "about" (\_ -> loadBody (fromFilePath "about.md"))     `mappend`
                    field "contact" (\_ -> loadBody (fromFilePath "contact.md")) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match (fromList ["about.md", "contact.md"]) $ do
      compile $ myPandocCompiler
        >>= loadAndApplyTemplate "templates/plain.html" postCtx
        >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- myRecentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    field "about" (\_ -> loadBody (fromFilePath "about.md"))     `mappend`
                    field "contact" (\_ -> loadBody (fromFilePath "contact.md")) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  pandocField "date" `mappend`
  pandocField "title" `mappend`
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

pandocField :: String -> Context a
pandocField f = field f $ \item -> do
  date <- loadSnapshot (itemIdentifier item) f
  return (itemBody date)

getPandocMeta :: Pandoc -> Meta
getPandocMeta (Pandoc meta _) = meta

fromDocDate :: Pandoc -> Maybe String
fromDocDate doc = do
  d <- listToMaybe [ date | (Str date) <- docDate (getPandocMeta doc)]
  t <- msum [ parseTime' format d | format <- formats ]
  return $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) t
  where
    parseTime' :: String -> String -> Maybe UTCTime
    parseTime' = parseTimeM True defaultTimeLocale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]

fromDocTitle :: Pandoc -> Maybe String
fromDocTitle doc =
  case unwords [ title | (Str title) <- docTitle (getPandocMeta doc)] of
    "" -> Nothing
    t  -> Just t

savePandocData :: Pandoc -> (Pandoc -> Maybe String) -> String -> Compiler ()
savePandocData doc fn field = do
  identifier <- getUnderlying
  metadata <- getMetadataField identifier field
  case msum [fn doc, metadata] of
    Nothing -> return ()
    Just value -> do
      item <- makeItem value
      _ <- saveSnapshot field item
      return ()

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWithTransformM ropt wopt $ \doc -> do
  savePandocData doc fromDocDate "date"
  savePandocData doc fromDocTitle "title"
  return doc
  where
    ropt = defaultHakyllReaderOptions
    wopt = defaultHakyllWriterOptions
      {
        writerHTMLMathMethod = MathML (Just ""),
        writerListings = True
      }

myChronological :: [Item a] -> Compiler [Item a]
myChronological = sortByM getTime
  where
    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs
    getTime :: Item a -> Compiler (Maybe UTCTime)
    getTime item = do
      date <- loadSnapshot (itemIdentifier item) "date"
      return $ case itemBody date of
                 "" -> Nothing
                 d -> (msum [ parseTime' format d | format <- formats])

    parseTime' :: String -> String -> Maybe UTCTime
    parseTime' = parseTimeM True defaultTimeLocale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]

myRecentFirst :: [Item a] -> Compiler [Item a]
myRecentFirst = liftM reverse . myChronological
