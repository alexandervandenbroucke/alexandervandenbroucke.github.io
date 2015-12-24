--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Data.List (sortBy)
import Text.Read (readMaybe)
import Data.Ord (Down(Down), comparing)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith conf $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler
  
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "fonts/*" $ do
    route idRoute
    compile copyFileCompiler

  match "publications/*" $ do
    route idRoute
    compile copyFileCompiler

  match "sections/*.markdown" $ do
    compile pandocCompiler

  match "index.html" $ do
    route idRoute
    compile $ do
      sections <- highestPriorityFirst =<< loadAll "sections/*"
      let ctx = listField "sections" defaultContext (return sections)
                `mappend` defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
type Priority = Maybe Int

highestPriorityFirst :: MonadMetadata m => [Item a] -> m [Item a]
highestPriorityFirst is = do
  priorities <- mapM (getPriority . itemIdentifier) is
  return (map fst (sortBy (comparing (Down . snd)) (zip is priorities)))

getPriority :: MonadMetadata m => Identifier -> m Priority
getPriority i = getMetadataField i "priority" >>= return . (>>= readMaybe)

--------------------------------------------------------------------------------
conf :: Configuration
conf = defaultConfiguration { deployCommand = "/bin/bash deploy.sh" }
