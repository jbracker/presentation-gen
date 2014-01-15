
module PresentationGen.Render
  ( htmlSlides, pandocToSlides
  ) where

import Data.List
import Data.Maybe
import Data.String

import Control.Monad

import Text.Blaze
import Text.Blaze.Html5 ( Html, toHtml, (!), toValue )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc

import System.FilePath ( (</>), (<.>) )

import PresentationGen.Types
import PresentationGen.Util

htmlSlides :: SlideM Html
htmlSlides = do
  title <- fromJust `fmap` getTitle
  authors <- getAuthors
  theme <- getTheme
  resourceDir <- getResourceDir
--  titleSlide <- htmlTitleSlide
  slideHead <- htmlSlideHead
  slides <- slidesToHtml 2
  return $H.docTypeHtml $ do
    slideHead
    H.body $ H.div ! A.class_ (toValue "reveal")
           $ H.div ! A.class_ (toValue "slides")
           $ do
--      titleSlide
      slides
      jsFile $ resourceDir </> "lib/js/head.min.js"
      jsFile $ resourceDir </> "js/reveal.min.js"
      jsSource $ intercalate "\n" $
        [ "Reveal.initialize({"
        , "controls: true,"
        , "progress: true,"
        , "history: true,"
        , "center: true,"
        , "theme: '" ++ theme ++ "',"
        , "transition: Reveal.getQueryHash().transition || 'default',"
        , "dependencies: ["
        , "{ src: '" ++ resourceDir </> "lib/js/classList.js', condition: function() { return !document.body.classList; } },"
        , "{ src: '" ++ resourceDir </> "plugin/markdown/showdown.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } },"
        --, "{ src: '" ++ resourceDir </> "plugin/markdown/markdown.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } },"
        , "{ src: '" ++ resourceDir </> "plugin/highlight/highlight.pack.js', async: true, callback: function() { hljs.initHighlightingOnLoad(); } },"
        , "{ src: '" ++ resourceDir </> "plugin/zoom-js/zoom.js', async: true, condition: function() { return !!document.body.classList; } },"
        , "{ src: '" ++ resourceDir </> "plugin/notes/notes.js', async: true, condition: function() { return !!document.body.classList; } }"
        , "]"
        , "});"
        ]

slidesToHtml :: Int -> SlideM Html
slidesToHtml initialHeadLevel = do
  slides <- getSlides
  mapGenHtml (slideToHtml initialHeadLevel) slides
  where
    slideToHtml :: Int -> Slide -> SlideM Html
    slideToHtml hl (Slide t c subs) = do
      headContent <- inlinesToHtml t
      content <- blocksToHtml c
      subSlides <- mapGenHtml (slideToHtml $ hl + 1) subs
      return $ H.section $ do
        (if null subs then id else H.section) $ do
          H.h2 $ headContent
          content
        subSlides

htmlSlideHead :: SlideM Html
htmlSlideHead = do
  authors <- (map fst) `fmap` getAuthors
  title <- fromJust `fmap` getTitle
  theme <- getTheme
  resourceDir <- getResourceDir
  slideDir <- getSlideDir
  slideName <- getSlideName
  return $ H.head $ do
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
    linkTag "stylesheet" $ resourceDir </> "css/reveal.min.css"
    linkTag "stylesheet" $ resourceDir </> "css/theme" </> theme <.>"css"
    -- Syntax highlight
    linkTag "stylesheet" $ resourceDir </> "lib/css/idea.css"
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
    linkTag "stylesheet" $ slideDir </> slideName <.> "css"

htmlTitleSlide :: SlideM Html
htmlTitleSlide = do
  title <- fromJust `fmap` getTitle
  authors <- map authorToHtml `fmap` getAuthors
  organisations <- getOrganisations
  return $ H.section $ do
    H.h1 $ fromString title
    H.p $ H.small $ do
      case length authors of
        0 -> return ()
        1 -> sequence_ authors
        n -> do
          sequence_ (intersperse (fromString ", ") (init authors))
          fromString " and "
          last authors
    seqMap orgToHtml organisations
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

-- -----------------------------------------------------------------------
-- Pandoc to Html Translation
-- -----------------------------------------------------------------------

pandocToSlides :: Pandoc -> [Slide]
pandocToSlides (Pandoc _ bs) = parseSlides bs

isHeader :: Int -> Block -> Bool
isHeader n (Header m _ _) | n == m = True
isHeader n _ = False

parseSlides :: [Block] -> [Slide]
parseSlides bs =
  let (_, headsConts) = groupSplit (isHeader 1) bs
  in fmap zipFunc headsConts
  where zipFunc :: (Block, [Block]) -> Slide
        zipFunc (Header n _ h, c) =
          let (cont, headsConts) = groupSplit (isHeader (n+1)) c
          in Slide h cont (fmap zipFunc headsConts)

mapGenHtml :: (a -> SlideM Html) -> [a] -> SlideM Html
mapGenHtml f l = mapM f l >>= foldM (\a b -> return $ a >> b) (return ())

blocksToHtml :: [Block] -> SlideM Html
blocksToHtml = mapGenHtml blockToHtml

blockToHtml :: Block -> SlideM Html
blockToHtml b = case b of
    Header n _ t -> do
      content <- inlinesToHtml t
      return $ header n $ content
    Plain t -> inlinesToHtml t
    Para t -> do
      content <- inlinesToHtml t
      return $ H.p $ content
    BulletList l -> do
      content <- mapGenHtml listItem l
      return $ H.ul $ content
    OrderedList _atts l -> do
      content <- mapGenHtml listItem l
      return $ H.ol $ content
    CodeBlock (_, hClasses, _) c -> do
      return $ H.pre $ H.code ! A.class_ (toValue $ intercalate " " hClasses)
                     $ toHtml c
    Null -> return $ return ()
    HorizontalRule -> return $ H.hr
    RawBlock tag s -> return $ preEscapedToMarkup s

  where
    listItem :: [Block] -> SlideM Html
    listItem i = do
      content <- blocksToHtml i
      return $ H.li $ content

inlinesToHtml :: [Inline] -> SlideM Html
inlinesToHtml = mapGenHtml inlineToHtml

inlineToHtml :: Inline -> SlideM Html
inlineToHtml l = case l of
  Str s -> return $ fromString s
  Space -> return $ fromString " "
  LineBreak -> return $ H.br
  Strong t -> do
    content <- inlinesToHtml t
    return $ H.strong $ content
  --Strikeout t -> do
  --  content <- inlinesToHtml t
  --  return $ H.strike $ content
  Superscript t -> do
    content <- inlinesToHtml t
    return $ H.sup $ content
  Subscript t -> do
    content <- inlinesToHtml t
    return $ H.sub $ content
  Emph t -> do
    content <- inlinesToHtml t
    return $ H.em $ content
  Code _ c -> return $ H.code $ fromString c
  RawInline _ s -> return $ preEscapedToMarkup s
  Image alt (url, t) -> do
    realUrl <- makeImageLink url
    return $ H.img ! A.alt (toValue $ concatMap inlineToString alt)
                   ! A.src (toValue $ realUrl)
                   ! A.title (toValue t)
  Link text (url, t) -> do
    content <- inlinesToHtml text
    return $ H.a ! A.href (toValue url)
                 ! A.title (toValue t)
                 $ content

inlineToString :: Inline -> String
inlineToString l = case l of
  Str s -> s
  Emph t -> concatMap inlineToString t
  Space -> " "
  Code _ c -> c
  Image alt (url, t) -> url

makeImageLink :: String -> SlideM String
makeImageLink img = do
  slideDir <- getSlideDir
  return $ if isExternalURI img
     then img
     else slideDir </> img

-- -----------------------------------------------------------------------
-- Html Short-Cuts
-- -----------------------------------------------------------------------

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

header :: Int -> Html -> Html
header 1 = H.h1
header 2 = H.h2
header 3 = H.h3
header 4 = H.h4
header 5 = H.h5
header 6 = H.h6
header n = id