
module PresentationGen.Util 
  ( groupSplit
  , isExternalURI
  , fieldFilter
  , separateBy
  , seqMap
  , trim
  ) where

import Data.List
import Data.Char

import Network.URI

-- | @groupSplit p l@ splits @l@ using the elements
--   that @p@ delivers true for as separators.
--   The result @(pre, seps, grps)@ consists of 
--   all elements before the first separator occured
--   @pre@ and the list of all found separators 
--   together with all elements that followed them up to the next
--   separator.
groupSplit :: (a -> Bool) -> [a] -> ([a], [(a,[a])])
groupSplit p [] = ([],[])
groupSplit p (y:xs) | p y = 
  let (z, rest) = span (not . p) xs
      (prefix, ys) = groupSplit p rest
  in (prefix, (y,z):ys)
groupSplit p xs = 
  let (prefix, rest) = span (not . p) xs
      (_, ys) = groupSplit p rest
  in (prefix, ys) 

-- | Check if the given string is a valid URI to a external location.
isExternalURI :: String -> Bool
isExternalURI s = maybe False id $ do
  uri <- parseURI s
  return $ uriIsAbsolute uri

-- | @fieldFilter ns field@ returns 'True' if the name 
--   of the given meta data pair @field@ is in the given list @ns@.
--   This is just a convenient function for filtering meta data.
fieldFilter :: [String] -> (String, String) -> Bool
fieldFilter field (n, _) = n `elem` field

-- | @s `separateBy` c@ cuts the string @s@ into pieces using
--   the character @c@ as separation marker.
separateBy :: String -> Char -> [String]
separateBy str sep =
  let (part, rest) = break (== sep) str
  in if null rest 
        then [part] 
        else part : separateBy (tail rest) sep

-- | @seqMap f l@ maps @f@ on each element of the list @l@
--   and then sequences the results within the monad.
seqMap :: (Monad m) => (a -> m b) -> [a] -> m ()
seqMap f l = sequence_ $ map f l

-- | Remove leading and trailing white spaces.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace







