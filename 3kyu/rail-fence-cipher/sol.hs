encode :: [a] -> Int -> [a]
encode [] _ = []
encode s n = go s n 0 1 [[]|_<-[1..n]] where
    go [] _ _ _ acc = concat acc   -- Join all rails and return
    go (x:xs) n i dir acc
        | dir==1 && n==i = go (x:xs) n (n-2) (-1) acc   -- Below bottom rail
        | dir==(-1) && i==(-1) = go (x:xs) n 1 1 acc    -- Above top rail
        | otherwise = go xs n (i+dir) dir (subNth i acc ((acc!!i)++[x]))


decode :: [Char] -> Int -> [Char]
decode [] _ = []
decode s n = go s n 0 1 0 0 (length s) ['@'|_<-[1..(length s)]] where
    go [] _ _ _ _ _ _ acc = acc 
    go (c:cs) n r dir x y len acc
        | y < 0 = go (c:cs) n r 1 x 1 len acc           -- Handle top bounce
        | y >= n = go (c:cs) n r (-1) x (n-2) len acc   -- Handle bottom bounce
        | x == len = go (c:cs) n (r+1) 1 0 0 len acc    -- Handle x overflow
        -- When y is on the target rail we push the next letter at acc!!x
        | r == y = go cs n r dir (x+1) (y+dir) len (subNth x acc c)
        | otherwise = go (c:cs) n r dir (x+1) (y+dir) len acc   -- advance x&y 

subNth :: Int -> [a] -> a -> [a]
subNth n l e = take n l++[e]++drop (n+1) l
