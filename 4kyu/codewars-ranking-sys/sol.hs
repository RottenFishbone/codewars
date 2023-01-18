-- https://www.codewars.com/kata/51fda2d95d6efda45e00004e
module CodewarRanking where

data User = User { rank :: Int, progress :: Int}
            deriving (Show)

newUser :: User
newUser = User (-8) 0

-- Applies increased progress based on kata rank
--  higher level kata give 10 * (rank diff)^2
--  equal level kata give 3
--  1 level lower give 1
--  otherwise no op
updateProg :: Int -> User -> User
updateProg diff u
    | diff >= 1 = User (rank u) (progress u + 10 * diff^2)
    | diff == 0 = User (rank u) (progress u + 3 )
    | diff == -1 = User (rank u) (progress u + 1 )
    | otherwise = u

-- Consumes 100 progress to increase rank, if possible
--  rank 8 does not progress
--  rank -1 jumps to 1 (if prog >= 100)
--  if prog >= 100 -> increment rank
--  otherwise no op
consumeProg :: User -> User
consumeProg u
    | rank u == 8 = User 8 0
    | rank u == -1 && progress u >= 100 = consumeProg (User 1 (progress u - 100))
    | progress u >= 100 = consumeProg (User (rank u + 1) (progress u - 100))
    | otherwise = u

-- Applies the score gain from solving a kata to user
incProgress :: Int -> User -> User
incProgress i u
    | i == 0 || i > 8 || i < -8 = error "Kata rank out of range"
    | rank u == 0 || rank u > 8 || rank u < -8 = error "Rank out of range"
    | (signum . rank) u /= signum i = -- Handle the -1 to 1 gap
        consumeProg $ updateProg (signum i * (abs(i - rank u) - 1)) u
    | otherwise = consumeProg $ updateProg (i - rank u) u


