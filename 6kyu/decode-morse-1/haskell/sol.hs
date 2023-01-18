{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Codewars.Kata.DecodeMorse (decodeMorse) where
import Data.Map.Strict ((!))
import Data.Map (Map, fromList)

-- NOTE: actual kata has a morseCodes map builtin but is unavailable to me
morseCodes = fromList[(" ", " "), ("-.-", "A"), (".", "E")]


-- Take characters from larger string until a space is found
-- Handles the case of leading "  " as " "
advance :: String -> (String, String)
advance [] = ([],[])
advance s = go s [] where
    go (' ':xs) [] = (" ", drop 1 xs)                   -- Handle spaces
    go (x:xs) acc
        | ' ' <- x = (morseCodes ! acc, xs)             -- Handle end of letter
        | [] <- xs = (morseCodes ! (acc ++ [x]), [])    -- Handle end of string
        | otherwise = go xs (acc ++ [x])                -- Append to current morse str


decodeMorse :: String -> String
decodeMorse s = go (stripStr s) [] where
    stripStr s = reverse $ dropWhile (== ' ') $ reverse $ dropWhile (== ' ') s
    go [] acc = acc
    go s acc = 
        let advanced = advance s in 
        go (snd advanced) (acc ++ fst advanced)


