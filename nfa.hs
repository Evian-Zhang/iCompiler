module NFA
( NFAState
, NFA
, nfa_states
, charset
, nfa_edges
, nfa_start_state
, nfa_end_state
, regular_tokens_to_NFA
, epsilon_closure_of_nfa_states
, terminal_closure_of_nfa_states
) where

import qualified Data.Set as Set
import qualified Data.List as List

import Re

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
                        , charset = Set.singleton $ CommonChar c
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

epsilon_closure_of_nfa_states :: NFA -> [NFAState] -> (Set.Set NFAState)
epsilon_closure_of_nfa_states nfa nfa_states = epsilon_closure_of_nfa_states' nfa nfa_states Set.empty

epsilon_closure_of_nfa_states' :: NFA -> [NFAState] -> (Set.Set NFAState) -> (Set.Set NFAState)
epsilon_closure_of_nfa_states' _ [] output = output
epsilon_closure_of_nfa_states' nfa nfa_states output = epsilon_closure_of_nfa_states' nfa nfa_states'' output'
            where
                nfa_state = head nfa_states
                nfa_states' = tail nfa_states
                output' = output `Set.union` (Set.fromList $ nfa_state : (nfa_edges nfa nfa_state Epsilon))
                new_states = output' Set.\\ output
                new_states' = Set.toList new_states
                nfa_states'' = nfa_states' ++ new_states'

terminal_closure_of_nfa_state :: NFA -> RECharType -> NFAState -> (Set.Set NFAState)
terminal_closure_of_nfa_state nfa c state = Set.fromList $ nfa_edges nfa state c

terminal_closure_of_nfa_states :: NFA -> RECharType -> (Set.Set NFAState) -> (Set.Set NFAState)
terminal_closure_of_nfa_states nfa c nfa_states = Set.foldl (\new_nfa_states nfa_state -> Set.union new_nfa_states $ terminal_closure_of_nfa_state nfa c nfa_state) Set.empty nfa_states