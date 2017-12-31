{--
Authors: Niraj Parajuli and Vitalii Stadnyk
Filename: SpellCheck.hs
--}

import System.IO
import System.Environment -- accessing arguments
import Data.Function (on) -- sort list of tuples based on second value
import Data.List (sortBy)
import Data.Char
import Data.Set hiding (map)

-- this function finds distance between 2 words
-- this function was implemented based on pseudocode from 
-- https://web.stanford.edu/class/cs124/lec/med.pdf
distance :: (Eq a) => [a] -> [a] -> Int
distance [] [] = 0
distance s1 [] = length(s1)
distance [] s2 = length(s2)
distance (x:xs) (y:ys) 
  | x == y = distance xs ys
  | otherwise = 1 + minimum [distance xs (y:ys), distance (x:xs) ys, distance xs ys]

-- The following is our own implementation of the Levenstein distance. 
-- Much faster. Slightly inaccurate

{--
powSet :: String -> [String]
powSet ""     = [""]
powSet (x:xs) = powSet' (powSet xs) (map (x:) (powSet xs)) where
                powSet' [] ys     = ys
                powSet' (x:xs) ys = x : (powSet' ys xs)

commonWords :: String -> String -> String
commonWords typedWord dictWord = [x | x <- typedWord, elem x dictWord]

boolList typed dict = zipWith ($) (map isSubsequenceOf (powSet (commonWords typed dict))) [dict | i <- [1..length(powSet (commonWords typed dict))]]

filterPowList typed dict = filter (/=[]) (zipWith (\x y -> if y then x else []) (powSet (commonWords typed dict)) (boolList typed dict))

maxSeq :: String -> String -> String
maxSeq typed dict = (filterPowList typed dict) !! ((\(Just x) -> x) (elemIndex (maximum (map length (filterPowList typed dict))) (map length (filterPowList typed dict))))

distance typed dict
    | commonWords typed dict == [] = max (length typed) (length dict)
    | otherwise = recurseDict typed dict (maxSeq typed dict)

mX (Just x) = x

recurseDict typed dict [] = max (length typed) (length dict)
recurseDict typed dict (x:xs) = distance (take (mX (elemIndex x typed)) typed) (take (mX (elemIndex x dict)) dict) + recurseDict (drop ((mX (elemIndex x typed))+1) typed) (drop ((mX (elemIndex x dict))+1) dict) xs
--}

-- this function checks if word is spelt correctly initially
spellCheck :: (Eq a, Foldable t) => a -> t a -> Bool
spellCheck wordToCheck dictionary
  | (wordToCheck `elem` dictionary) == True = True
  | otherwise = False

-- this function sorts list of tuples (word, distance)
sortDistances :: Ord b => [(a, b)] -> [(a, b)] 
sortDistances = reverse.sortBy (flip compare `on` snd)

-- this function produces list of best suggestions
suggestions :: [(a,b)] -> [a]
suggestions sug = [fst(x) | x <- sug]

-- this function writes output to a file
writeToFile :: FilePath -> String -> String -> IO ()
writeToFile file word suggestedWords = appendFile file (word ++ " : " ++ suggestedWords ++ "\n")

-- this function goes through the file with words to be checked regarding spelling
-- and checks if an action is required
forAllWords :: [String] -> [String] -> FilePath -> Int -> IO () 
forAllWords [] _ outputFile _ = putStrLn ("Check " ++ outputFile ++ "!")
forAllWords (wordToCheck:xs) dictionary outputFile x = do
  if spellCheck wordToCheck dictionary then
    do
      forAllWords xs dictionary outputFile x
  else do
    let distances = [(x, distance wordToCheck x) | x <- dictionary]
    writeToFile outputFile wordToCheck (foldr1 (\x y -> x ++" "++y) (suggestions (take x (sortDistances distances))))
    forAllWords xs dictionary outputFile x

main :: IO ()
main = do
  let topSuggestions = 10
  args <- getArgs
  content <- readFile (args!!0)
  textPunc <- readFile (args!!1)
  let outputFile = args!!2
  let text = [if x `elem` "-,.?;:$%`()[]{}<>/\\\"!_+=" then ' ' else toLower x | x <- textPunc]
  let dictionary = (toList.fromList) [map toLower x | x <- lines content] -- A and a are removed, so no multiple suggestion
  let test = (toList.fromList) [map toLower x | x <- words text, foldr1 (||) ((map isAlpha x))] -- misspelled words only appear once in results file
  forAllWords test dictionary outputFile topSuggestions
