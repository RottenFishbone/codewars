import Data.Char (isSpace)

data Token = Operator Char
           | Value Double
           deriving (Eq, Show)

-- List of valid operators, grouped by precedence
operators :: [[Token]]
operators = [[Operator '*', Operator '/'], [Operator '+', Operator '-']]

-- Parse a mathematical expression into a result
calc :: String -> Double
calc = evalStr . filter (not . isSpace)

-- Evaluates a math expression string into a Double
evalStr :: String -> Double
evalStr = go [] where
    go acc [] = evalTokens acc  -- evaluate parsed tokens into Double
    go acc ('(':xs) = let (expr, tail) = isolateParen ('(':xs) 
                      in go (acc++[Value (evalStr expr)]) tail
    go [] s = let (val, tail) = isolateNum s -- Expressions always start with Value
              in go [Value val] tail

    go acc (x:xs)
        | '+' <- x = go (acc++[Operator '+']) xs
        | '*' <- x = go (acc++[Operator '*']) xs
        | '/' <- x = go (acc++[Operator '/']) xs
        | '-' <- x = case last acc of
                -- Parse as operator if preceding token is a value (or empty)
                (Value _) -> go (acc++[Operator '-']) xs
                -- Otherwise it must be a (negated) value
                (Operator _) -> let (val, tail) = isolateNum (x:xs)
                                in go (acc++[Value val]) tail
        | otherwise = let (val, tail) = isolateNum (x:xs)
                      in go (acc++[Value val]) tail

-- Evaluates a token list into a result using `operators` for order of ops
evalTokens :: [Token] -> Double
evalTokens [Value val] = val
evalTokens [Value a, Operator op, Value b] = applyBinOperator a op b
evalTokens t = applyOpSet operators t where
    applyOps :: [Token] -> [Token] -> [Token] -> [Token]
    applyOps [] _ t = t     -- No operators
    applyOps _ acc [] = acc -- No tokens left
    applyOps ops acc ((Value a):(Operator o):(Value b):tail)
        -- If `o` is in the set of valid operators, consume o(a,b) and push to t
        | Operator o `elem` ops = let v = Value (applyBinOperator a o b)
                         in applyOps ops acc (v:tail)
        -- Otherwise, skip o(a,b) and retain b for next set of tokens
        | otherwise = applyOps ops (acc++[Value a, Operator o]) (Value b:tail)
    applyOps ops acc t = acc++t     -- Insuff tokens to do math, just push and return
    
    -- Iterate through operators, consuming them in `t` until only a value remains
    applyOpSet :: [[Token]] -> [Token] -> Double
    applyOpSet _ [Value val] = val
    applyOpSet [] _ = error "Unresolved tokens remain"
    applyOpSet (op:ops) t = applyOpSet ops (applyOps op [] t)


applyBinOperator :: Double -> Char -> Double -> Double
applyBinOperator a op b = apply a op b where
    apply a '+' b = a + b
    apply a '-' b = a - b
    apply a '*' b = a * b
    apply a '/' b = a / b
    apply _ _ _ = error "Invalid Operator"

-- Extracts a number from the front of a string
isolateNum :: String -> (Double, String)
isolateNum = go [] where
    go acc [] = (read acc, [])
    -- Negate any number following '-' (including more negations)
    go [] ('-':xs) = let (val, tail) = go [] xs
                     in (negate val, tail)
    -- Evaluate expression into a number and return it
    go [] ('(':xs) = let (expr, tail) = isolateParen ('(':xs)
                      in (evalStr expr, tail)
    go acc (x:xs) 
        | x `elem` "-+*/" = (read acc, x:xs)    -- Parse on reading an operator
        | otherwise = go (acc++[x]) xs          -- else, Add to acc to build num string


-- Extracts a parenthesised expression from the front of a string
isolateParen :: String -> (String, String)
isolateParen [] = ([], [])
isolateParen s = go [] 1 (tail s) where
    go acc 0 tail = (init acc, tail)
    go acc n ('(':xs) = go (acc++['(']) (n+1) xs
    go acc n (')':xs) = go (acc++[')']) (n-1) xs
    go acc n (x:xs) = go (acc++[x]) n xs
    go acc _ [] = error "Mismatched parenthesis"
