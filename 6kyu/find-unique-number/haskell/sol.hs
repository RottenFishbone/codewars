module CodeWars.UniqueNumber where
import Data.List (group, sort)

getUnique :: [Int] -> Int
getUnique l = (head . head) $ filter (\g -> length g == 1) $ (group . sort) l
