
{-# LANGUAGE OverloadedStrings #-}


module MetaParser 
  ( parseMeta, parseMetaFile 
  , getTitle, getAuthors, getOrganisations
  , separateBy
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Text ( unpack, Text, pack )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Attoparsec.Text ( Parser, isEndOfLine )
import qualified Data.Attoparsec.Text as P

import Control.Monad
import Control.Applicative

import Debug.Trace

getAuthors :: [(String, String)] -> [(String, [Int])]
getAuthors meta = map (\(auth,a) -> (auth, map (read . trim) $ separateBy a ','))
                $ combineAuthorAssocs
                $ filter (fieldFilter ["Author", "Associated"]) meta
  where
    combineAuthorAssocs :: [(String, String)] -> [(String, String)]
    combineAuthorAssocs (("Author", auth) : ("Associated", assoc) : xs) = 
      (auth, assoc) : combineAuthorAssocs xs
    combineAuthorAssocs (("Author", auth) : xs) = 
      (auth, "") : combineAuthorAssocs xs
    combineAuthorAssocs (x : xs) = combineAuthorAssocs xs
    combineAuthorAssocs [] = []

getOrganisations :: [(String, String)] -> [(Int, [String])]
getOrganisations meta = zip [1..]
                      $ map ((`separateBy` '\n') . snd)
                      $ filter (fieldFilter ["Organisation"]) meta

getTitle :: [(String, String)] -> Maybe String
getTitle meta = listToMaybe 
              $ map snd 
              $ filter (fieldFilter ["Title"]) meta

fieldFilter :: [String] -> (String, String) -> Bool
fieldFilter field (n, _) = n `elem` field

separateBy :: String -> Char -> [String]
separateBy str sep =
  let (part, rest) = break (== sep) str
  in if null rest 
        then [part] 
        else part : separateBy (tail rest) sep

-- ----- META INFORMATION PARSER --------------------------------         

parseMetaFile :: FilePath -> IO (Either String [(String, String)])
parseMetaFile file = parseMeta <$> T.readFile file

parseMeta :: Text -> Either String [(String, String)]
parseMeta t = P.parseOnly pFields t 

pFields :: Parser [(String, String)]
pFields = do 
  fields <- many  (  fmap (const Nothing) pEmptyLine 
                 <|> fmap Just            pField
                 <|> fmap (const Nothing) pComment
                  )
  P.skipSpace
  P.endOfInput
  return $ map fromJust $ filter isJust fields

pComment :: Parser ()
pComment = do
  pSkipSpace
  P.char '#'
  P.skipWhile (not . isEndOfLine)
  pLineEnd

pField :: Parser (String, String)
pField = do
  P.skipSpace
  name <- pFieldIdent
  P.skipSpace
  pFieldSep
  value <- pFieldLineValue <|> pFieldMultilineValue
  return (name, value)

pFieldIdent :: Parser String
pFieldIdent = unpack <$> P.takeTill (\c -> isSpace c || c == ':')

pFieldLineValue :: Parser String
pFieldLineValue = do
  value <- unpack <$> P.takeWhile1 (not . isEndOfLine)
  when (all isSpace value) $ fail "Value only contains spaces!"
  pLineEnd
  return $ trim $ value

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

pFieldMultilineValue :: Parser String
pFieldMultilineValue = do
  pEmptyLine
  indent <- pSkipSpace
  l  <- pFieldLineValue
  ls <- many (pIndentLine indent)
  return $ trim $ intercalate "\n" $ l : ls

pFieldSep :: Parser ()
pFieldSep = P.char ':' *> pure ()

pEmptyLine :: Parser ()
pEmptyLine = pSkipSpaceNotLineEnd >> pLineEnd

pSkipSpaceNotLineEnd :: Parser ()
pSkipSpaceNotLineEnd = P.skipWhile isSpaceNotLineEnd

pSkipSpace :: Parser Int
pSkipSpace = T.length <$> P.takeWhile isSpaceNotLineEnd

isSpaceNotLineEnd :: Char -> Bool
isSpaceNotLineEnd c = isSpace c && not (isEndOfLine c)

pIndentLine :: Int -> Parser String
pIndentLine n = (pEmptyLine *> pure "") <|> do
  isIndent <- all isSpaceNotLineEnd . unpack <$> P.take n
  when (not isIndent) $ fail "Wrong indentation!"
  pFieldLineValue

pLineEnd :: Parser ()
pLineEnd  =  (P.string "\r\n" *> pure ()) 
         <|> (P.char '\r' *> pure ()) 
         <|> (P.char '\n' *> pure ())







