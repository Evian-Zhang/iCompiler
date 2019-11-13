module NFA
( NFAState (NFAState)
, NFA (NFA)
, nfa_states
, nfa_charset
, nfa_edges
, nfa_start_state
, nfa_end_state
, regular_tokens_to_NFA
) where

import qualified Data.Set as Set
import qualified Data.List as List

import Re

data NFAState = NFAState Int deriving (Eq, Ord)

instance Show NFAState where
    show (NFAState index) = show index

shift_state :: Int->NFAState->NFAState
shift_state index (NFAState x) = NFAState (x + index)

data NFA = NFA { nfa_states :: [NFAState]
               , nfa_charset :: Set.Set RECharType
               , nfa_edges :: (NFAState -> RECharType -> [NFAState])
               , nfa_start_state :: NFAState
               , nfa_end_state :: NFAState
               }

instance Show NFA where
    show nfa = states_str ++ "\n" ++ start_state_str ++ "\n" ++ end_state_str ++ "\n" ++ edges_str
        where
            nfa_states' = List.sort $ nfa_states nfa
            states_str = "States: " ++ (show nfa_states')
            start_state_str = "Start state: " ++ (show $ nfa_start_state nfa)
            end_state_str = "End state: " ++ (show $ nfa_end_state nfa)
            nfa_charset' = Epsilon : (Set.toList $ nfa_charset nfa)
            edges_str = show_state_edges nfa_states' nfa_charset' (nfa_edges nfa)
                where
                    show_state_edges nfa_states nfa_charset nfa_edges = if List.null nfa_states
                        then ""
                        else (show_state_char_edges (head nfa_states) nfa_charset nfa_edges) ++ (show_state_edges (tail nfa_states) nfa_charset nfa_edges)
                            where
                                show_state_char_edges _ [] _ = ""
                                show_state_char_edges nfa_state charset nfa_edges = (case next_states of
                                    [] -> "" 
                                    xs -> show_state_char_edge nfa_state (head charset) xs) ++ show_state_char_edges nfa_state (tail charset) nfa_edges
                                    where
                                        next_states = nfa_edges nfa_state $ head charset
                                        show_state_char_edge _ _ [] = ""
                                        show_state_char_edge nfa_state char next_states = "\n" ++ show nfa_state ++ " -" ++ (show char) ++ "-> " ++ (show $ head next_states) ++ show_state_char_edge nfa_state char (tail next_states)


shift_NFA :: Int -> NFA -> NFA
shift_NFA index nfa = 
    NFA { nfa_states = nfa_states'
        , nfa_charset = nfa_charset nfa
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
                        , nfa_charset = Set.singleton $ CommonChar c
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
-- @param operator  binary operator
-- @param lchild    left operand of operator
-- @param rchild    right operand of opeartor
binary_operator_to_NFA :: REOperatorType -> NFA -> NFA -> NFA
binary_operator_to_NFA operator lchild rchild = case operator of
    And ->
        NFA { nfa_states = nfa_states'
            , nfa_charset = nfa_charset lchild `Set.union` (nfa_charset rchild)
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
                                                    then 
                                                        if c == Epsilon
                                                            then nfa_start_state rchild' : (nfa_edges lchild (NFAState index) c)
                                                            else nfa_edges rchild' (NFAState index) c
                                                    else nfa_edges rchild' (NFAState index) c
    Or -> 
        NFA { nfa_states = nfa_states'
            , nfa_charset = nfa_charset lchild `Set.union` (nfa_charset rchild)
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
                                if index == lend_index && c == Epsilon
                                    then nfa_end_state' : (nfa_edges lchild' (NFAState index) c)
                                    else
                                        if index < rend_index
                                            then nfa_edges rchild' (NFAState index) c
                                            else
                                                if index == rend_index && c == Epsilon
                                                    then nfa_end_state' : (nfa_edges rchild' (NFAState index) c)
                                                    else []
    _ -> error "Unexpected error"

-- unary_operator_to_NFA operator child
-- @param operator  unary operator
-- @param child     operand of operator
unary_operator_to_NFA :: REOperatorType -> NFA -> NFA
unary_operator_to_NFA operator child = case operator of
    Repeat -> 
        NFA { nfa_states = nfa_states'
            , nfa_charset = nfa_charset child
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
                if index == 0 && c == Epsilon
                    then [nfa_start_state child', nfa_end_state']
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
-- @param tokens    regular tokens
-- @param s         output stack
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
