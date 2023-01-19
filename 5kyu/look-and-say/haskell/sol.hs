module LookAndSay where
import Data.List (group)

lookSay :: Integer -> Integer
lookSay = read . concatMap (\g -> (show . length) g ++ [head g]) . group . show
