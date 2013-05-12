
module PresentationGen.Types
  ( Slide(..), SlideConfig
  , MetaData
  , SlideM
  , getBaseDir, getResourceDir, getSlideDir
  , getSlideName, getSlides, getSlideMetaData
  , getTitle, getAuthors, getOrganisations, getTheme
  , makeSlideConfig
  , setSlides, setSlideMetaData
  ) where

import Data.Maybe

import Control.Monad.State

import Text.Pandoc ( Pandoc(..), Block(..), Inline(..) )

import PresentationGen.Util

-- -----------------------------------------------------------------------
-- Basic Slide Types
-- -----------------------------------------------------------------------

data Slide = Slide
  { slideTitle :: [Inline]
  , slideContent :: [Block]
  , subSlides :: [Slide]
  } deriving Show

type MetaData = [(String, String)]

data SlideConfig = SlideConfig
  { baseDir_ :: FilePath
  , resourceDir_ :: FilePath
  , slideName_ :: String
  , slideDir_ :: FilePath
  , slides_ :: [Slide]
  , slideMetaData_ :: MetaData
  }

-- | Make a slide configuration from the Markdown and
--   meta file with the given slide name in the given
--   slide directory. The resource directory is the place
--   shared resource like presentation JS and CSS file stored.
--   Images are searched for in the slide directory.
--   Both directories are relative to the base dir.
--   Call: @makeSlideConfig baseDir slideName slideDir resourceDir@
makeSlideConfig :: FilePath -> String -> FilePath -> FilePath -> SlideConfig
makeSlideConfig baseDir slideName slideDir resourceDir = SlideConfig
  { baseDir_ = baseDir
  , resourceDir_ = resourceDir
  , slideName_ = slideName
  , slideDir_ = slideDir
  , slides_ = []
  , slideMetaData_ = []
  }

-- -----------------------------------------------------------------------
-- SlideM Monad
-- -----------------------------------------------------------------------

type SlideM a = StateT SlideConfig IO a

getBaseDir :: SlideM FilePath
getBaseDir = fmap baseDir_ get

getResourceDir :: SlideM FilePath
getResourceDir = fmap resourceDir_ get

getSlideName :: SlideM String
getSlideName = fmap slideName_ get

getSlideDir :: SlideM FilePath
getSlideDir = fmap slideDir_ get

getSlides :: SlideM [Slide]
getSlides = fmap slides_ get

getSlideMetaData :: SlideM MetaData
getSlideMetaData = fmap slideMetaData_ get

setSlides :: [Slide] -> SlideM ()
setSlides slides = do
  config <- get
  put $ config { slides_ = slides }

setSlideMetaData :: MetaData -> SlideM ()
setSlideMetaData meta = do
  config <- get
  put $ config { slideMetaData_ = meta }

getAuthors :: SlideM [(String, [Int])]
getAuthors = do
  meta <- getSlideMetaData
  return $ map (\(auth,a) -> (auth, map (read . trim) $ separateBy a ','))
         $ combineAuthorAssocs
         $ filter (fieldFilter ["Author", "Associated"]) meta
  where
    combineAuthorAssocs :: MetaData -> MetaData
    combineAuthorAssocs (("Author", auth) : ("Associated", assoc) : xs) =
      (auth, assoc) : combineAuthorAssocs xs
    combineAuthorAssocs (("Author", auth) : xs) =
      (auth, "") : combineAuthorAssocs xs
    combineAuthorAssocs (x : xs) = combineAuthorAssocs xs
    combineAuthorAssocs [] = []

getOrganisations :: SlideM [(Int, [String])]
getOrganisations = do
  meta <- getSlideMetaData
  return $ zip [1..]
         $ map ((`separateBy` '\n') . snd)
         $ filter (fieldFilter ["Organisation"]) meta

getTitle :: SlideM (Maybe String)
getTitle = do
  return (Just "Slides")
{-
  meta <- getSlideMetaData
  return $ listToMaybe
         $ map snd
         $ filter (fieldFilter ["Title"]) meta
-}

getTheme :: SlideM String
getTheme = do
  return "sky"
{-
  meta <- getSlideMetaData
  return $ maybe "default" id
         $ listToMaybe
         $ map snd
         $ filter (fieldFilter ["Theme"]) meta

-}



