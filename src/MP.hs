module MP (separators, lookUp, splitText, combine, getKeywordDefs, expand) where

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

{-|
This function will look up a key in a list of key-value pairs,
returning all the values that match with that key.

> lookUp "A" [("A", 8), ("B", 9), ("C", 5), ("A", 7)] == [8, 7]
-}
lookUp :: String -> [(String, a)] -> [a]
lookUp y [] = []
lookUp y xs = [b | (a,b) <- xs , a == y]

{-|
This function will break up a string with some given separator
characters, returning both the list of separators found between
each "word" and the words themselves.
-}
splitText :: [Char] -- ^ the separators to split on
          -> String -- ^ the string to split
          -> ([Char], [String])
splitText k [] = ([] , [""])
splitText k (c:cs)
  | elem c k  = (c:seps, "":word:words) 
  | otherwise = (seps , (c:word):words)
  where 
    (seps, word:words) = splitText k cs 
   
 
{-|
This function interleaves the characters from the first argument
list with the strings in the second argument. The second list must
be non-empty.
-}
combine :: [Char] -> [String] -> [String]
combine [] []         = []
combine [] (k:ks)     = k:combine [] ks 
combine (c:cs) []     = [c]:combine cs [] 
combine (c:cs) (k:ks) = k:[c]:combine cs ks  


{-|
This function takes a list of lines and splits each line to
extract a list of keyword-definition pairs.

> getKeywordDefs ["$x Define x", "$y 55"] == [("$x", "Define x"), ("$y", "55")]
-}
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs []       = [] 
getKeywordDefs (str:strs) = (word, words_new):getKeywordDefs strs 
  where
    (_:seps,word:words) = splitText [' '] str
    words_new             = concat(combine seps words)

{-|  
This function takes the contents of two files, one containing
a template and the other the definitions for each keyword
found within the template. It extracts the keyword-definition
information from the info file and uses it to expand the occurrences
of these keywords in the template file, producing new file contents
as a result.

> expand "The capital $1 is $2" "$1 Peru\n$2 Lima." == "The capital of Peru is Lima"
-}
expand :: FileContents -- ^ the template file contents
       -> FileContents -- ^ the info file contents
       -> FileContents
expand t i = concat (combine s [replaceWord x def | x <- w])
  where 
    (s,w) = splitText separators t
    ni    = snd(splitText "\n" i)
    def   = getKeywordDefs ni

-- You may wish to uncomment and implement this helper function
-- when implementing expand
    replaceWord :: String -> KeywordDefs -> String
    replaceWord k@('$':_) def = head (lookUp k def)
    replaceWord k def         = k



