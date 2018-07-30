--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Data.List (sortBy)
import Text.Read (readMaybe)
import Data.Ord (Down(Down), comparing)
import System.FilePath (replaceDirectory)

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

  match "talks/*" $ do
    route idRoute
    compile copyFileCompiler

  match "AppProb/index.markdown" $ do
    compile $ do
      ctx <- sectionCtx
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" ctx

  create ["AppProb/index.html","app/index.html"] $ do
    route idRoute
    compile $ do
      loadBody "AppProb/index.markdown"
        >>= makeItem
        >>= relativizeUrls

  match "AppProb/*" $ do
    route idRoute
    compile copyFileCompiler

  match "AppProb/*" $ do
    route (setDirectory "app/")
    compile copyFileCompiler

  match "sections/*.markdown" $ do
    compile pandocCompiler

  match "index.html" $ do
    route idRoute
    compile $ do
      ctx <- sectionCtx
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
type Priority = Maybe Double

sectionCtx :: Compiler (Context String)
sectionCtx = do
  sections <- highestPriorityFirst =<< loadAll "sections/*"
  return $ listField "sections" defaultContext (return sections)
           `mappend` defaultContext


highestPriorityFirst :: MonadMetadata m => [Item a] -> m [Item a]
highestPriorityFirst = sortByM (getPriority . itemIdentifier)
  
sortByM :: (Ord b, Monad m) => (a -> m b) -> [a] -> m [a]
sortByM toKeyM l = do
  keys <- mapM toKeyM l
  return $ map fst $ sortBy (comparing (Down . snd)) $ zip l keys

getPriority :: MonadMetadata m => Identifier -> m Priority
getPriority i = fmap (>>= readMaybe) (getMetadataField i "priority")

--------------------------------------------------------------------------------
conf :: Configuration
conf = defaultConfiguration { deployCommand = "/bin/bash deploy.sh" }

--------------------------------------------------------------------------------
setDirectory :: FilePath -> Routes
setDirectory dir = customRoute ((`replaceDirectory` dir) .  toFilePath)

-- Local Variables:
-- dante-repl-command-line: ("cabal" "new-repl" dante-project-root)
-- End:
