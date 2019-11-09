import qualified Data.Set as Set
import qualified Data.List as List

data REOperatorType = And | Or | Repeat deriving (Eq, Show)
data RECharType = CommonChar Char | Epsilon deriving (Eq, Show)
data REToken = REChar RECharType | REOperator REOperatorType | ParenOpen | ParenClose deriving (Eq, Show)

tokenize_regular_char :: Char -> REToken
tokenize_regular_char operator = case operator of
    '.' -> REOperator And
    '|' -> REOperator Or
    '*' -> REOperator Repeat
    '(' -> ParenOpen
    ')' -> ParenClose
    c   -> REChar (CommonChar c)

tokenize_regular_expression :: String -> [REToken]
tokenize_regular_expression xs = foldl (\tokens c -> tokens ++ [tokenize_regular_char c]) [] xs

priority :: REToken -> Int
priority token = case token of
    REOperator Or -> 0
    REOperator And -> 1
    REOperator Repeat -> 2
    ParenOpen -> 3
    ParenClose -> 3
    REChar _ -> 4

is_operator :: REToken -> Bool
is_operator token = case token of
    REOperator _ -> True
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
        REOperator operator ->
            if s /= [] && is_operator (head s) && priority (head s) > priority (REOperator operator)
                then shunting_yard' xs (tail s) (q ++ [head s])
                else shunting_yard' remain (REOperator operator : s) q
        ParenOpen -> shunting_yard' remain (ParenOpen : s) q
        ParenClose ->
            if s /= [] && head s /= ParenOpen
                then shunting_yard' xs (tail s) (q ++ [head s])
                else shunting_yard' remain (tail s) q

data State = State Int deriving (Eq, Ord, Show)

data NFA = NFA { states :: [State]
               , edges :: (State -> RECharType -> [State])
               , start_state :: State
               , end_state :: State
               }

char_to_NFA :: RECharType -> NFA
char_to_NFA c = case c of
    CommonChar c -> NFA { states = [start_state', end_state']
                        , edges = edges'
                        , start_state = start_state'
                        , end_state = end_state'
                        }
                    where
                        start_state' = State 0
                        end_state' = State 1
                        edges' state char =
                            if state == start_state' && char == CommonChar c
                                then [end_state']
                                else []
    Epsilon -> error "Unexpcted error"

-- binary_operator_to_NFA operator lchild rchild
-- @param operator binary operator
-- @param lchild left operand of operator
-- @param rchild right operand of opeartor
binary_operator_to_NFA :: REOperatorType -> NFA -> NFA -> NFA
binary_operator_to_NFA operator lchild rchild = case operator of
    And ->
        NFA { states = states'
            , edges = edges'
            , start_state = start_state'
            , end_state = end_state'
            }
        where
            start_state' = start_state lchild
            end_state' = end_state rchild
            State lend_index = end_state'
            rchild_states = states rchild
            rchild_states' = List.map (\(State previous) -> (State (previous + lend_index + 1))) rchild_states'
            states' = states lchild ++ rchild_states'
            rchild_edges = edges rchild
            rchild_edges' = \(State index) c -> List.map (\(State previous_index) -> (State (previous_index + lend_index + 1))) (rchild_edges (State (index + lend_index + 1)) c)
            edges' = \(State index) c -> if index <= lend_index
                                            then edges lchild (State index) c
                                            else rchild_edges' (State index) c
    Or -> error "Unexpected error"
    Repeat -> error "Unexpected error"


-- regular_expression_to_NFA :: [REToken] -> NFA
-- regular_expression_to_NFA [] = NFA {states=Set.empty, edges=(\_ _ -> Set.empty), start_state=}

charset :: [REToken] -> [REToken]
charset [] = []
charset (x:xs) = case x of
    REChar c -> 
        if elem (REChar c) remainCharset
            then remainCharset
            else (REChar c) : remainCharset
        where remainCharset = charset xs
    _ -> charset xs