module PowerModN (modPow) where

-- Recursive exponentiation by squaring
-- https://en.wikipedia.org/wiki/Exponentiation_by_squaring 
modPow :: Integer -> Integer -> Integer -> Integer
modPow 0 _ _ = 0            -- 0^n = 0
modPow n e m = go (n `mod` m) e m (1 `mod` m) where
    go _ 0 _ acc = acc      -- Exit once exponent is depleted
    go n e m acc            -- Square and mod for every set bit in e
        | odd e = go (n^2 `mod` m) (e `div` 2) m ((acc * n) `mod` m) 
        | otherwise = go (n^2 `mod` m) (e `div` 2) m acc
