-- https://www.codewars.com/kata/54b72c16cd7f5154e9000457
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Kata.DecodeMorseAdvanced where
import Data.Map (fromList)
import Data.Map.Strict ((!))

import Data.List.Split (splitOn)
import Data.List (minimum, group, intercalate)

morseCodes = fromList [(".", "e"), ("....", "h"), ("-.--", "y"), (".---","j"),("..-", "u"),("-..", "d")]

-- Takes a string of morse-encoded bits and converts into a morse encoded ascii string
decodeBits :: String -> String
decodeBits bits = let
    b = trimElem '0' bits
    ts = findTimescale b
    words = splitUnit ts 7 b
    chars = map (splitUnit ts 3) words
    in intercalate "   " $ map (unwords . map (map (bitsToDots ts) . splitUnit ts 1)) chars where
    splitUnit ts tu = splitOn (replicate (ts * tu) '0')
    bitsToDots ts b = if length b == ts then '.' else '-'
    findTimescale = minimum . map length . group

-- Takes ascii morse-encoded string and converts to readable text
decodeMorse :: String -> String
decodeMorse s = go (trimElem ' ' s) [] where
    go [] acc = acc
    go s acc =
        let advanced = advanceMorse s in
        go (snd advanced) (acc ++ fst advanced)

-- Removes leading and trailing characters in string
trimElem :: Eq a => a -> [a] -> [a]
trimElem v = reverse . dropWhile (== v) . reverse . dropWhile (== v)

-- Advances a string by taking the next complete morse-encoded character
advanceMorse :: String -> (String, String)
advanceMorse [] = ([],[])
advanceMorse s = go s [] where
    go (' ':xs) [] = (" ", drop 1 xs)                   -- Handle spaces
    go (x:xs) acc
        | ' ' <- x = (morseCodes ! acc, xs)             -- Handle end of letter
        | [] <- xs = (morseCodes ! (acc ++ [x]), [])    -- Handle end of string
        | otherwise = go xs (acc ++ [x])                -- Append to current morse str


