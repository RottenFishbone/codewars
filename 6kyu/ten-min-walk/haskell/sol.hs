module Codewars.Kata.TenMinuteWalk where
isValidWalk :: [Char] -> Bool
isValidWalk w = go w 0 (0,0) where
    go _ 11 _ = False        
    go [] i acc = acc == (0,0) && i == 10   
    go (x:xs) i acc
        | 'n' <- x = go xs (i+1) (fst acc + 1, snd acc)
        | 's' <- x = go xs (i+1) (fst acc - 1, snd acc)
        | 'e' <- x = go xs (i+1) (fst acc, snd acc + 1)
        | 'w' <- x = go xs (i+1) (fst acc, snd acc - 1)
        | otherwise = False
