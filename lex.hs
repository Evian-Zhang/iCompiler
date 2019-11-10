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

data NFAState = NFAState Int deriving (Eq, Ord, Show)

shift_state :: Int->NFAState->NFAState
shift_state index (NFAState x) = NFAState (x + index)

data NFA = NFA { nfa_states :: [NFAState]
               , charset :: Set.Set RECharType
               , nfa_edges :: (NFAState -> RECharType -> [NFAState])
               , nfa_start_state :: NFAState
               , nfa_end_state :: NFAState
               }

shift_NFA :: Int -> NFA -> NFA
shift_NFA index nfa = 
    NFA { nfa_states = nfa_states'
        , charset = charset nfa
        , nfa_edges = nfa_edges'
        , nfa_start_state = nfa_start_state'
        , nfa_end_state = nfa_end_state'
        }
    where
        nfa_start_state' = shift_state index $ nfa_start_state nfa
        nfa_end_state' = shift_state index $ nfa_end_state nfa
        nfa_states' = List.map (shift_state index) (nfa_states nfa)
        nfa_edges' = \state c -> List.map (shift_state index) $ nfa_edges nfa (shift_state (negate index) state) c

char_to_NFA :: RECharType -> NFA
char_to_NFA c = case c of
    CommonChar c -> NFA { nfa_states = [nfa_start_state', nfa_end_state']
                        , charset = Set.Singleton c
                        , nfa_edges = nfa_edges'
                        , nfa_start_state = nfa_start_state'
                        , nfa_end_state = nfa_end_state'
                        }
                    where
                        nfa_start_state' = NFAState 0
                        nfa_end_state' = NFAState 1
                        nfa_edges' state char =
                            if state == nfa_start_state' && char == CommonChar c
                                then [nfa_end_state']
                                else []
    Epsilon -> error "Unexpcted error"

-- binary_operator_to_NFA operator lchild rchild
-- @param operator binary operator
-- @param lchild left operand of operator
-- @param rchild right operand of opeartor
binary_operator_to_NFA :: REOperatorType -> NFA -> NFA -> NFA
binary_operator_to_NFA operator lchild rchild = case operator of
    And ->
        NFA { nfa_states = nfa_states'
            , charset = charset lchild `Set.union` (charset rchild)
            , nfa_edges = nfa_edges'
            , nfa_start_state = nfa_start_state'
            , nfa_end_state = nfa_end_state'
            }
        where
            nfa_start_state' = nfa_start_state lchild
            NFAState lend_index = nfa_end_state lchild
            rchild' = shift_NFA (lend_index + 1) rchild
            nfa_end_state' = nfa_end_state rchild'
            rchild_nfa_states' = nfa_states rchild'
            nfa_states' = nfa_states lchild ++ rchild_nfa_states'
            nfa_edges' = \(NFAState index) c -> if index < lend_index
                                            then nfa_edges lchild (NFAState index) c
                                            else 
                                                if index == lend_index
                                                    then nfa_start_state rchild' : (nfa_edges lchild (NFAState index) c)
                                                    else nfa_edges rchild' (NFAState index) c
    Or -> 
        NFA { nfa_states = nfa_states'
            , charset = charset lchild `Set.union` (charset rchild)
            , nfa_edges = nfa_edges'
            , nfa_start_state = nfa_start_state'
            , nfa_end_state = nfa_end_state'
            }
        where
            nfa_start_state' = NFAState 0
            lchild' = shift_NFA 1 lchild
            NFAState lend_index = nfa_end_state lchild'
            rchild' = shift_NFA (lend_index + 1) rchild
            NFAState rend_index = nfa_end_state rchild'
            nfa_end_state' = NFAState (rend_index + 1)
            nfa_states' = nfa_start_state' : (nfa_end_state' : ((nfa_states lchild') ++ (nfa_states rchild')))
            nfa_edges' = \(NFAState index) c -> 
                if index == 0
                    then if c == Epsilon then [nfa_start_state lchild', nfa_start_state rchild'] else []
                    else 
                        if index < lend_index 
                            then nfa_edges lchild' (NFAState index) c
                            else
                                if index < rend_index
                                    then nfa_edges rchild' (NFAState index) c
                                    else
                                        if index == rend_index && c == Epsilon
                                            then nfa_end_state' : (nfa_edges lchild' (NFAState index) c) ++ (nfa_edges rchild' (NFAState index) c)
                                            else []
    _ -> error "Unexpected error"

-- unary_operator_to_NFA operator child
-- @param operator unary operator
-- @param child operand of operator
unary_operator_to_NFA :: REOperatorType -> NFA -> NFA
unary_operator_to_NFA operator child = case operator of
    Repeat -> 
        NFA { nfa_states = nfa_states'
            , charset = charset child
            , nfa_edges = nfa_edges'
            , nfa_start_state = nfa_start_state'
            , nfa_end_state = nfa_end_state'
            }
        where
            nfa_start_state' = NFAState 0
            child' = shift_NFA 1 child
            NFAState end_index' = nfa_end_state child'
            nfa_end_state' = NFAState (end_index' + 1)
            nfa_states' = nfa_start_state' : (nfa_end_state' : (nfa_states child'))
            nfa_edges' = \(NFAState index) c ->
                if index == 0
                    then if c == Epsilon then [nfa_start_state child'] else []
                    else
                        if index < end_index'
                            then nfa_edges child' (NFAState index) c
                            else 
                                if index == end_index' && c == Epsilon
                                    then nfa_end_state' : (nfa_start_state child' : (nfa_edges child' (NFAState index) c))
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

epsilon_closure_of_nfa_state :: NFA -> NFAState -> [NFAState]
epsilon_closure_of_nfa_state nfa state = nfa_edges nfa state Epsilon

terminal_closure_of_nfa_state :: NFA -> RECharType -> NFAState -> [NFAState]
terminal_closure_of_nfa_state nfa c state = nfa_edges nfa state c

data DFAState = DFAState Int (Set.Set NFAState)

instance Ord DFAState where
    compare (DFAState _ nfa_states1) (DFAState _ nfa_states2) = compare nfa_states1 nfa_states2

epsilon_closure_of_nfa_states :: NFA -> (Set.Set NFAState) -> (Set.Set NFAState)
epsilon_closure_of_nfa_states nfa nfa_states = Set.foldl (\new_nfa_states nfa_state -> Set.union new_nfa_states (Set.fromList $ epsilon_closure_of_nfa_state nfa nfa_state)) Set.empty nfa_states

terminal_closure_of_nfa_states :: NFA -> RECharType -> (Set.Set NFAState) -> (Set.Set NFAState)
terminal_closure_of_nfa_states nfa c nfa_states = Set.foldl (\new_nfa_states nfa_state -> Set.union new_nfa_states (Set.fromList $ terminal_closure_of_nfa_state nfa c nfa_state)) Set.empty nfa_states

data DFA = DFA { dfa_states :: Set.Set DFAState
               , dfa_edges :: (DFAState -> RECharType -> (Set.Set DFAState))
               , dfa_start_state :: DFAState
               , dfa_end_states :: Set.Set DFAState
               }

single_dfa :: DFAState -> DFA
single_dfa dfa_state = DFA { dfa_states = Set.singleton dfa_state
                           , dfa_edges = (\_ _ -> [])
                           , dfa_start_state = dfa_state
                           , dfa_end_states = []
                           }

nfa_to_dfa :: NFA -> DFA
nfa_to_dfa nfa = nfa_to_dfa' nfa  m single_dfa initial_dfa_state
    where
        initial_dfa_state = DFAState 0 $ epsilon_closure_of_nfa_state nfa $ start_state nfa
        m = Set.singleton initial_dfa_state

-- insert' dfa_state dfa_states
-- if dfa_state is in dfa_states, just do nothing
-- if dfa_state is not in dfa_states, assign the size of dfa_states to the index of dfa_state and insert it
insert' :: DFAState -> (Set.Set DFAState) -> (Set.Set DFAState)
insert' dfa_state@(DFAState _ nfa_states) dfa_states =
    if Set.member dfa_state dfa_states
        then dfa_states
        else
            Set.insert (DFAState (size dfa_states) nfa_states) dfa_states

-- nfa_to_dfa' nfa processing dfa
-- @param nfa the NFA to be converted from
-- @param processing a set of DFAState to be processed
-- @param dfa current DFA
nfa_to_dfa' :: NFA -> [DFAState] -> DFA -> DFA
nfa_to_dfa' nfa [] dfa = dfa
nfa_to_dfa' nfa processing dfa = nfa_to_dfa' nfa processing' dfa'
    where
        dfa_state = head processing
        processing' = tail processing
        dfa' = nfa_to_dfa'' nfa (charset nfa) dfa_state dfa

nfa_to_dfa'' :: NFA -> [RECharType] -> DFAState -> DFA -> DFA
nfa_to_dfa'' nfa [] dfa_state dfa = dfa
nfa_to_dfa'' nfa charset dfa_state dfa = nfa (tail charset) dfa_state dfa'
        where
            c = head charset
            DFAState index nfa_states = dfa_state
            encounted = dfa_states dfa
            next_nfa_states = epsilon_closure_of_nfa_states . terminal_closure_of_nfa_states c nfa_states
            next_dfa_state = DFAState (size encounted) next_nfa_states
            (next_dfa_state', encounted') = case Set.lookupIndex next_nfa_state encounted of
                                                Int index' -> (Set.elemAt index' encounted, encounted)
                                                False -> (next_dfa_state, Set.insert next_dfa_state encounted)
            dfa_edges' = (\the_state@(DFAState the_index _) the_c ->
                            if the_index == index && the_c == c
                                then Set.insert next_dfa_state' (dfa_edges dfa the_state c)
                                else dfa_edges dfa the_state c
                         )
            dfa_end_states' = if List.length $ List.filter (\nfa_state -> nfa_end_state nfa == nfa_state) next_nfa_states
                                then Set.insert next_Dfa_state $ dfa_end_states dfa
                                else dfa_end_states dfa
            dfa' = DFA { dfa_states = encounted'
                       , dfa_edges = dfa_edges'
                       , dfa_start_state = dfa_start_state dfa
                       , dfa_end_states = dfa_end_states'
                       }