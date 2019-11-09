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
            if not (null s) && is_operator (head s) && priority (head s) > priority (REOperator operator)
                then shunting_yard' xs (tail s) (q ++ [head s])
                else shunting_yard' remain (REOperator operator : s) q
        ParenOpen -> shunting_yard' remain (ParenOpen : s) q
        ParenClose ->
            if not (null s) && head s /= ParenOpen
                then shunting_yard' xs (tail s) (q ++ [head s])
                else shunting_yard' remain (tail s) q

data State = State Int deriving (Eq, Ord, Show)

shift_state :: Int->State->State
shift_state index (State x) = State (x + index)

data NFA = NFA { states :: [State]
               , edges :: (State -> RECharType -> [State])
               , start_state :: State
               , end_state :: State
               }

shift_NFA :: Int -> NFA -> NFA
shift_NFA index nfa = 
    NFA { states = states'
        , edges = edges'
        , start_state = start_state'
        , end_state = end_state'
        }
    where
        start_state' = shift_state index $ start_state nfa
        end_state' = shift_state index $ end_state nfa
        states' = List.map (shift_state index) (states nfa)
        edges' = \state c -> List.map (shift_state index) $ edges nfa (shift_state (negate index) state) c

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
            State lend_index = end_state lchild
            rchild' = shift_NFA (lend_index + 1) rchild
            end_state' = end_state rchild'
            rchild_states' = states rchild'
            states' = states lchild ++ rchild_states'
            edges' = \(State index) c -> if index < lend_index
                                            then edges lchild (State index) c
                                            else 
                                                if index == lend_index
                                                    then start_state rchild' : (edges lchild (State index) c)
                                                    else edges rchild' (State index) c
    Or -> 
        NFA { states = states'
            , edges = edges'
            , start_state = start_state'
            , end_state = end_state'
            }
        where
            start_state' = State 0
            lchild' = shift_NFA 1 lchild
            State lend_index = end_state lchild'
            rchild' = shift_NFA (lend_index + 1) rchild
            State rend_index = end_state rchild'
            end_state' = State (rend_index + 1)
            states' = start_state' : (end_state' : ((states lchild') ++ (states rchild')))
            edges' = \(State index) c -> 
                if index == 0
                    then if c == Epsilon then [start_state lchild', start_state rchild'] else []
                    else 
                        if index < lend_index 
                            then edges lchild' (State index) c
                            else
                                if index < rend_index
                                    then edges rchild' (State index) c
                                    else
                                        if index == rend_index && c == Epsilon
                                            then end_state' : (edges lchild' (State index) c) ++ (edges rchild' (State index) c)
                                            else []
    _ -> error "Unexpected error"

-- unary_operator_to_NFA operator child
-- @param operator unary operator
-- @param child operand of operator
unary_operator_to_NFA :: REOperatorType -> NFA -> NFA
unary_operator_to_NFA operator child = case operator of
    Repeat -> 
        NFA { states = states'
            , edges = edges'
            , start_state = start_state'
            , end_state = end_state'
            }
        where
            start_state' = State 0
            child' = shift_NFA 1 child
            State end_index' = end_state child'
            end_state' = State (end_index' + 1)
            states' = start_state' : (end_state' : (states child'))
            edges' = \(State index) c ->
                if index == 0
                    then if c == Epsilon then [start_state child'] else []
                    else
                        if index < end_index'
                            then edges child' (State index) c
                            else 
                                if index == end_index' && c == Epsilon
                                    then end_state' : (start_state child' : (edges child' (State index) c))
                                    else []
    _ -> error "Unexpected error"

regular_tokens_to_NFA :: [REToken] -> NFA
-- regular_tokens_to_NFA' tokens s
-- @param tokens regular tokens
-- @param s output stack
regular_tokens_to_NFA tokens = regular_tokens_to_NFA' tokens [] where
    regular_tokens_to_NFA' [] [nfa] = nfa
    regular_tokens_to_NFA' [] _ = error "Regular expression not valid!"
    regular_tokens_to_NFA' xs@(x:remain) s = case x of
        REChar c -> regular_tokens_to_NFA' remain (char_to_NFA c : s)
        REOperator And -> case s of
            (rchild : lchild : s_remain) -> regular_tokens_to_NFA' remain $ binary_operator_to_NFA And lchild rchild : s_remain
            _ -> error "Regular expression not valid!"
        REOperator Or -> case s of
            (rchild : lchild : s_remain) -> regular_tokens_to_NFA' remain $ binary_operator_to_NFA Or lchild rchild : s_remain
            _ -> error "Regular expression not valid!"
        REOperator Repeat -> case s of 
            (child : s_remain) -> regular_tokens_to_NFA' remain $ unary_operator_to_NFA Repeat child : s_remain
            _ -> error "Regular expression not valid!"
        _ -> error "Unexpected error"


charset :: [REToken] -> [REToken]
charset [] = []
charset (x:xs) = case x of
    REChar c -> 
        if elem (REChar c) remainCharset
            then remainCharset
            else (REChar c) : remainCharset
        where remainCharset = charset xs
    _ -> charset xs