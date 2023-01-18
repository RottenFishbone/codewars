module High.JorgeVS.Kata where
import Data.Char (ord)
import Data.List (maximumBy)
import Data.Ord (comparing)
-- Split the string on ' ', take maximum by summing ascii values (- 96) and comparing 
-- used reverse to take the first word instead of last equal word as max
high s = maximumBy (comparing (foldr ((+) . (\e -> e - 96) . ord) 0)) ((reverse . words) s)

