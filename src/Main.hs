
{-# LANGUAGE OverloadedStrings #-}

import Data.Default
import Data.List
import Data.String
import Data.Maybe

import Control.Monad

import System.Directory
import System.FilePath ( (</>), (<.>) )
import System.Environment

import MetaParser
import SlideCreator

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
    createSlides config





