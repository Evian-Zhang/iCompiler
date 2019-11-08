import Data.Map hiding (foldl)

data REOperator = And | Or | Repeat deriving (Eq, Show)
data REToken = REChar Char | Operator REOperator | ParenOpen | ParenClose deriving (Eq, Show)

tokenize_regular_char :: Char -> REToken
tokenize_regular_char operator = case operator of
    '.' -> Operator And
    '|' -> Operator Or
    '*' -> Operator Repeat
    '(' -> ParenOpen
    ')' -> ParenClose
    c   -> REChar c

tokenize_regular_expression :: String -> [REToken]
tokenize_regular_expression xs = foldl (\tokens c -> tokens ++ [tokenize_regular_char c]) [] xs

priority :: REToken -> Int
priority token = case token of
    Operator Or -> 0
    Operator And -> 1
    Operator Repeat -> 2
    ParenOpen -> 3
    ParenClose -> 3
    REChar _ -> 4

is_operator :: REToken -> Bool
is_operator token = case token of
    Operator _ -> True
    _ -> False
    
shunting_yard :: [REToken] -> [REToken]
-- shunting_yard' x s q
-- @param x tokens
-- @param s operator stack, with top at left
-- @param q output queue, with front at left
shunting_yard x = shunting_yard' x [] [] where
    shunting_yard' [] [] q = q
    shunting_yard' [] s q =
        if head s == ParenOpen
            then error "Mismatched Parentheses"
            else shunting_yard' [] (tail s) (q ++ [head s])
    shunting_yard' xs@(x:remain) s q = case x of
        REChar c -> shunting_yard' remain s (q ++ [REChar c])
        Operator operator ->
            if s /= [] && is_operator (head s) && priority (head s) > priority (Operator operator)
                then shunting_yard' xs (tail s) (q ++ [head s])
                else shunting_yard' remain (Operator operator : s) q
        ParenOpen -> shunting_yard' remain (ParenOpen : s) q
        ParenClose ->
            if head s /= ParenOpen
                then shunting_yard' xs (tail s) (q ++ [head s])
                else shunting_yard' remain (tail s) q