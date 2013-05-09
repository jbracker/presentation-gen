
{-# LANGUAGE OverloadedStrings #-}

import Data.Default
import Data.List
import Data.String
import Data.Maybe

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import System.Directory
import System.FilePath ( (</>), (<.>) )
import System.Environment

import Text.Pandoc.Readers.Markdown
import Text.Blaze.Html.Renderer.String ( renderHtml )

import PresentationGen.MetaParser
import PresentationGen.Types
import PresentationGen.Util
import PresentationGen.Render

main :: IO ()
main = do
  args <- getArgs
  let baseDir = if null args then "./" else head args
      presDir = "presentations"
      resDir  = "resources"
  preses <- getDirectoryContents $ baseDir </> presDir
  forM_ (filter ((/='.') . head) preses) $ \pres -> do
    putStrLn $ "CREATING: " ++ pres
    let config = makeSlideConfig baseDir pres (presDir </> pres) resDir
    generateSlides config



generateSlides :: SlideConfig -> IO ()
generateSlides config =
  (flip evalStateT) config $ do
    baseDir <- getBaseDir
    slideName <- getSlideName
    slideDir <- getSlideDir
    let slideFile = baseDir </> slideDir </> slideName <.> "md"
    liftIO $ putStrLn $ "Reading slides from: " ++ slideFile
    slideMarkdown <- liftIO $ readFile slideFile
    liftIO $ putStrLn $ "Parsing slide pandoc..."
    let slidePandoc = readMarkdown def slideMarkdown
        slides = pandocToSlides slidePandoc
        metaFile = baseDir </> slideDir </> slideName <.> "meta"
    liftIO $ putStrLn $ "Reading meta data from: " ++ metaFile
    metaE <- liftIO $ parseMetaFile metaFile
    case metaE of
      Left e -> liftIO $ do
        putStrLn $ "Failed to parse meta data for " ++ slideName
        putStrLn $ "ERROR: " ++ e
      Right meta -> do
        liftIO $ putStrLn $ "Creating HTML data for slides..."
        setSlides slides
        setSlideMetaData meta
        html <- htmlSlides
        let targetFile = baseDir </> slideName <.> "html"
        liftIO $ do
          putStrLn $ "Writing slides to: " ++ targetFile
          writeFile targetFile $ renderHtml html
          return ()

