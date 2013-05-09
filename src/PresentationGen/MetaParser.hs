
{-# LANGUAGE OverloadedStrings #-}


module PresentationGen.MetaParser 
  ( parseMeta, parseMetaFile
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

import PresentationGen.Types
import PresentationGen.Util

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







