
module SlideCreator
  ( Slide(..), SlideConfig
  , makeSlideConfig, createSlides
  ) where

import Data.Default
import Data.List
import Data.String
import Data.Maybe

import Control.Monad

import Text.Blaze
import Text.Blaze.Html.Renderer.String ( renderHtml )
import Text.Blaze.Html5 ( Html, toHtml, (!), toValue )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Pandoc
import Text.Pandoc.Readers.Markdown

import System.FilePath ( (</>), (<.>) )

import Debug.Trace

import MetaParser

data Slide = Slide 
  { slideTitle :: [Inline]
  , slideContent :: [Block]
  , subSlides :: [Slide]
  } deriving Show

data SlideConfig = SlideConfig
  { baseDir_ :: FilePath
  , resourceDir_ :: FilePath
  , slideName_ :: String
  , slideDir_ :: FilePath
  , slides_ :: [Slide]
  , slideMetaData_ :: [(String, String)]
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

createSlides :: SlideConfig -> IO ()
createSlides config = do
  let baseDir = baseDir_ config
      slideName = slideName_ config
      slideDir = slideDir_ config
      slideFile = baseDir </> slideDir </> slideName <.> "md"
  putStrLn $ "Reading slides from: " ++ slideFile
  slideMarkdown <- readFile slideFile
  putStrLn $ "Parsing slide pandoc..."
  let slidePandoc = readMarkdown def slideMarkdown
      slides = pandocToSlides slidePandoc
      metaFile = baseDir </> slideDir </> slideName <.> "meta"
  putStrLn $ "Reading meta data from: " ++ metaFile
  metaE <- parseMetaFile metaFile
  case metaE of
    Left e -> do
      putStrLn $ "Failed to parse meta data for " ++ slideName
      putStrLn $ "ERROR: " ++ e
    Right meta -> do
      putStrLn $ "Creating HTML data for slides..."
      let slideHtml = presentation $ config 
                      { slides_ = slides
                      , slideMetaData_ = meta }
          targetFile = baseDir </> slideName <.> "html"
      putStrLn $ "Writing slides to: " ++ targetFile
      writeFile targetFile $ renderHtml slideHtml
      return ()

pandocToSlides :: Pandoc -> [Slide]
pandocToSlides (Pandoc _ bs) = parseSlides bs

presentation :: SlideConfig -> Html
presentation config = H.docTypeHtml $ do
  let meta = slideMetaData_ config
      slides = slides_ config
      title = fromJust $ getTitle meta
      authors = getAuthors meta
      resources = resourceDir_ config
  slidesHead config
  H.body $ H.div ! A.class_ (toValue "reveal")
         $ H.div ! A.class_ (toValue "slides")
         $ do
    titleSlide meta
    slidesToHtml 2 slides
    jsFile $ resources </> "lib/js/head.min.js"
    jsFile $ resources </> "js/reveal.min.js"
    jsSource $ intercalate "\n" $
      [ "Reveal.initialize({"
      , "controls: true,"
      , "progress: true,"
      , "history: true,"
      , "center: true,"
      , "theme: 'sky',"
      , "transition: Reveal.getQueryHash().transition || 'default',"
      , "dependencies: ["
      , "{ src: '" ++ resources </> "lib/js/classList.js', condition: function() { return !document.body.classList; } },"
      , "{ src: '" ++ resources </> "plugin/markdown/showdown.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } },"
      --, "{ src: '" ++ resources </> "plugin/markdown/markdown.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } },"
      , "{ src: '" ++ resources </> "plugin/highlight/highlight.js', async: true, callback: function() { hljs.initHighlightingOnLoad(); } },"
      , "{ src: '" ++ resources </> "plugin/zoom-js/zoom.js', async: true, condition: function() { return !!document.body.classList; } },"
      , "{ src: '" ++ resources </> "plugin/notes/notes.js', async: true, condition: function() { return !!document.body.classList; } }"
      , "]"
      , "});"
       ]

titleSlide :: [(String, String)] -> Html
titleSlide meta = H.section $ do
  let title = fromJust $ getTitle meta
      authors = map authorToHtml $ getAuthors meta
      organisations = getOrganisations meta
  H.h1 $ fromString title
  H.p $ H.small $ do
    case length authors of
      0 -> return ()
      1 -> sequence_ authors
      n -> do
        sequence_ (intersperse (fromString ", ") (init authors))
        fromString " and " 
        last authors
  sequence_ $ map orgToHtml organisations
  where
    authorToHtml :: (String, [Int]) -> Html
    authorToHtml (auth, []) = fromString auth
    authorToHtml (auth, assoc) = do
      fromString auth 
      H.sup $ fromString $ intercalate "," $ map show assoc 
    
    orgToHtml :: (Int, [String]) -> Html
    orgToHtml (n, org) = H.p $ H.small $ H.address $ do
      H.sup $ toHtml n
      sequence_ $ intersperse H.br $ map fromString org

groupSplit :: (a -> Bool) -> [a] -> ([a], [a], [[a]])
groupSplit p [] = ([],[],[])
groupSplit p (y:xs) | p y = 
  let (z, rest) = span (not . p) xs
      (prefix, ys, zs) = groupSplit p rest
  in (prefix, y:ys, z:zs)
groupSplit p xs = 
  let (prefix, rest) = span (not . p) xs
      (_, ys, zs) = groupSplit p rest
  in (prefix, ys, zs)

isHeader :: Int -> Block -> Bool
isHeader n (Header m _ _) | n == m = True
isHeader n _ = False

parseSlides :: [Block] -> [Slide]
parseSlides bs =
  let (_, heads, conts) = groupSplit (isHeader 1) bs
  in zipWith zipFunc heads conts
  where zipFunc :: Block -> [Block] -> Slide
        zipFunc (Header n _ h) c = 
          let (cont, heads, conts) = groupSplit (isHeader (n+1)) c
          in Slide h cont (zipWith zipFunc heads conts)
  
slidesToHtml :: Int -> [Slide] -> Html
slidesToHtml initialHeadLevel slides = 
    listToHtml (slideToHtml initialHeadLevel) slides
  where
    slideToHtml :: Int -> Slide -> Html
    slideToHtml hl (Slide t c subs) = H.section $ do
      (if null subs then id else H.section) $ do
        header hl $ listToHtml inlineToHtml t
        listToHtml blockToHtml c
      listToHtml (slideToHtml $ hl + 1) subs

listToHtml :: (a -> Html) -> [a] -> Html
listToHtml f l = sequence_ $ map f l

header :: Int -> Html -> Html
header 1 = H.h1
header 2 = H.h2
header 3 = H.h3
header 4 = H.h4
header 5 = H.h5
header 6 = H.h6
header n = id

inlineToHtml :: Inline -> Html
inlineToHtml l = case l of
  Str s -> fromString s
  Space -> fromString " "
  LineBreak -> H.br
  Strong t -> H.strong $ listToHtml inlineToHtml t
  --Strikeout t -> H.strike $ listToHtml inlineToHtml t
  Superscript t -> H.sup $ listToHtml inlineToHtml t
  Subscript t -> H.sub $ listToHtml inlineToHtml t
  Emph t -> H.em $ listToHtml inlineToHtml t
  Code _ c -> H.code $ fromString c
  RawInline _ s -> H.code $ fromString s
  Image alt (url, t) -> H.img ! A.alt (toValue $ concatMap inlineToString alt)
                              ! A.src (toValue url)
                              ! A.title (toValue t)
  Link text (url, t) -> H.a ! A.href (toValue url)
                            ! A.title (toValue t)
                            $ listToHtml inlineToHtml text

inlineToString :: Inline -> String
inlineToString l = case l of
  Str s -> s
  Emph t -> concatMap inlineToString t
  Space -> " "
  Code _ c -> c
  Image alt (url, t) -> url

blockToHtml :: Block -> Html
blockToHtml b = case b of
    Header n _ t -> header n $ listToHtml inlineToHtml t
    Plain t -> listToHtml inlineToHtml t
    Para t -> H.p $ listToHtml inlineToHtml t
    BulletList l -> H.ul $ listToHtml listItem l
    OrderedList _atts l -> H.ol $ listToHtml listItem l
    CodeBlock (_, hClasses, _) c -> do
      --trace (show atts) $ return ()
      H.pre $ H.code ! A.class_ (toValue $ intercalate " " hClasses) 
            $ toHtml c
    Null -> return ()
    HorizontalRule -> H.hr
  where
    listItem :: [Block] -> Html
    listItem i = H.li $ listToHtml blockToHtml i

metaTag :: String -> String -> Html
metaTag name content = H.meta ! A.name (toValue name) 
                              ! A.content (toValue content)

linkTag :: String -> String -> Html
linkTag rel href = H.link ! A.rel (toValue rel) 
                          ! A.href (toValue href)

jsFile :: FilePath -> Html
jsFile file = H.script ! A.type_ (toValue "text/javascript")
                       ! A.src (toValue file) 
                       $ return ()

jsSource :: String -> Html
jsSource src = H.script ! A.type_ (toValue "text/javascript") 
                        $ fromString src

slidesHead :: SlideConfig -> Html
slidesHead config = H.head $ do
  let meta = slideMetaData_ config
      title = fromJust $ getTitle meta
      authors = map fst $ getAuthors meta
      resources = resourceDir_ config
      slidePath = slideDir_ config
  -- Title
  H.title $ fromString title
  -- Charset
  H.meta ! A.charset (toValue "utf-8")
  -- Meta information
  metaTag "description" title
  metaTag "author" (intercalate ", " authors )
  -- Apple specific
  metaTag "apple-mobile-web-app-capable" "yes"
  metaTag "apple-mobile-web-app-status-bar-style" "black-translucent"
  -- Viewport
  metaTag "viewport" "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
  -- Reveal.js styles
  linkTag "stylesheet" $ resources </> "css/reveal.min.css"
  linkTag "stylesheet" $ resources </> "css/theme/default.css"
  -- Syntax highlight
  linkTag "stylesheet" $ resources </> "lib/css/zenburn.css"
  {- Add at some point
  <!-- If the query includes 'print-pdf', use the PDF print sheet -->
  <script>
    document.write( '<link rel="stylesheet" href="css/print/' + ( window.location.search.match( /print-pdf/gi ) ? 'pdf' : 'paper' ) + '.css" type="text/css" media="print">' );
  </script>
  
  <!--[if lt IE 9]>
  <script src="lib/js/html5shiv.js"></script>
  <![endif]-->
  -}
  -- Custom CSS
  linkTag "stylesheet" $ slidePath </> slideName_ config <.> "css"