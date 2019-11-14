module NFA
( NFAState (NFAState)
, NFA (NFA)
, nfa_states
, nfa_charset
, nfa_edges
, nfa_start_state
, nfa_end_states
, nfa_id
, regular_expressions_to_NFA
) where

import qualified Data.Set as Set
import qualified Data.List as List

import Re

data NFAState = NFAState Int deriving (Eq, Ord)

instance Show NFAState where
    show (NFAState index) = show index

shift_state :: Int->NFAState->NFAState
shift_state index (NFAState x) = NFAState (x + index)

data SingleNFA = SingleNFA { single_nfa_states :: [NFAState]
                           , single_nfa_charset :: Set.Set RECharType
                           , single_nfa_edges :: (NFAState -> RECharType -> [NFAState])
                           , single_nfa_start_state :: NFAState
                           , single_nfa_end_state :: NFAState
                           }

shift_SingleNFA :: Int -> SingleNFA -> SingleNFA
shift_SingleNFA index single_nfa = 
    SingleNFA { single_nfa_states = single_nfa_states'
              , single_nfa_charset = single_nfa_charset single_nfa
              , single_nfa_edges = single_nfa_edges'
              , single_nfa_start_state = single_nfa_start_state'
              , single_nfa_end_state = single_nfa_end_state'
              }
    where
        single_nfa_start_state' = shift_state index $ single_nfa_start_state single_nfa
        single_nfa_end_state' = shift_state index $ single_nfa_end_state single_nfa
        single_nfa_states' = List.map (shift_state index) (single_nfa_states single_nfa)
        single_nfa_edges' = \state c -> List.map (shift_state index) $ single_nfa_edges single_nfa (shift_state (negate index) state) c

char_to_SingleNFA :: RECharType -> SingleNFA
char_to_SingleNFA c = case c of
    CommonChar c -> SingleNFA { single_nfa_states = [single_nfa_start_state', single_nfa_end_state']
                              , single_nfa_charset = Set.singleton $ CommonChar c
                              , single_nfa_edges = single_nfa_edges'
                              , single_nfa_start_state = single_nfa_start_state'
                              , single_nfa_end_state = single_nfa_end_state'
                              }
                    where
                        single_nfa_start_state' = NFAState 0
                        single_nfa_end_state' = NFAState 1
                        single_nfa_edges' state char =
                            if state == single_nfa_start_state' && char == CommonChar c
                                then [single_nfa_end_state']
                                else []
    Epsilon -> error "Unexpcted error"

-- binary_operator_to_SingleNFA operator lchild rchild
-- @param operator  binary operator
-- @param lchild    left operand of operator
-- @param rchild    right operand of opeartor
binary_operator_to_SingleNFA :: REOperatorType -> SingleNFA -> SingleNFA -> SingleNFA
binary_operator_to_SingleNFA operator lchild rchild = case operator of
    And ->
        SingleNFA { single_nfa_states = single_nfa_states'
                  , single_nfa_charset = single_nfa_charset lchild `Set.union` (single_nfa_charset rchild)
                  , single_nfa_edges = single_nfa_edges'
                  , single_nfa_start_state = single_nfa_start_state'
                  , single_nfa_end_state = single_nfa_end_state'
                  }
        where
            single_nfa_start_state' = single_nfa_start_state lchild
            NFAState lend_index = single_nfa_end_state lchild
            rchild' = shift_SingleNFA (lend_index + 1) rchild
            single_nfa_end_state' = single_nfa_end_state rchild'
            rchild_single_nfa_states' = single_nfa_states rchild'
            single_nfa_states' = single_nfa_states lchild ++ rchild_single_nfa_states'
            single_nfa_edges' = \(NFAState index) c -> if index < lend_index
                                            then single_nfa_edges lchild (NFAState index) c
                                            else 
                                                if index == lend_index
                                                    then 
                                                        if c == Epsilon
                                                            then single_nfa_start_state rchild' : (single_nfa_edges lchild (NFAState index) c)
                                                            else single_nfa_edges rchild' (NFAState index) c
                                                    else single_nfa_edges rchild' (NFAState index) c
    Or -> 
        SingleNFA { single_nfa_states = single_nfa_states'
                  , single_nfa_charset = single_nfa_charset lchild `Set.union` (single_nfa_charset rchild)
                  , single_nfa_edges = single_nfa_edges'
                  , single_nfa_start_state = single_nfa_start_state'
                  , single_nfa_end_state = single_nfa_end_state'
                  }
        where
            single_nfa_start_state' = NFAState 0
            lchild' = shift_SingleNFA 1 lchild
            NFAState lend_index = single_nfa_end_state lchild'
            rchild' = shift_SingleNFA (lend_index + 1) rchild
            NFAState rend_index = single_nfa_end_state rchild'
            single_nfa_end_state' = NFAState (rend_index + 1)
            single_nfa_states' = single_nfa_start_state' : (single_nfa_end_state' : ((single_nfa_states lchild') ++ (single_nfa_states rchild')))
            single_nfa_edges' = \(NFAState index) c -> 
                if index == 0
                    then if c == Epsilon then [single_nfa_start_state lchild', single_nfa_start_state rchild'] else []
                    else 
                        if index < lend_index 
                            then single_nfa_edges lchild' (NFAState index) c
                            else
                                if index == lend_index && c == Epsilon
                                    then single_nfa_end_state' : (single_nfa_edges lchild' (NFAState index) c)
                                    else
                                        if index < rend_index
                                            then single_nfa_edges rchild' (NFAState index) c
                                            else
                                                if index == rend_index && c == Epsilon
                                                    then single_nfa_end_state' : (single_nfa_edges rchild' (NFAState index) c)
                                                    else []
    _ -> error "Unexpected error"

-- unary_operator_to_SingleNFA operator child
-- @param operator  unary operator
-- @param child     operand of operator
unary_operator_to_SingleNFA :: REOperatorType -> SingleNFA -> SingleNFA
unary_operator_to_SingleNFA operator child = case operator of
    Repeat -> 
        SingleNFA { single_nfa_states = single_nfa_states'
                  , single_nfa_charset = single_nfa_charset child
                  , single_nfa_edges = single_nfa_edges'
                  , single_nfa_start_state = single_nfa_start_state'
                  , single_nfa_end_state = single_nfa_end_state'
                  }
        where
            single_nfa_start_state' = NFAState 0
            child' = shift_SingleNFA 1 child
            NFAState end_index' = single_nfa_end_state child'
            single_nfa_end_state' = NFAState (end_index' + 1)
            single_nfa_states' = single_nfa_start_state' : (single_nfa_end_state' : (single_nfa_states child'))
            single_nfa_edges' = \(NFAState index) c ->
                if index == 0 && c == Epsilon
                    then [single_nfa_start_state child', single_nfa_end_state']
                    else
                        if index < end_index'
                            then single_nfa_edges child' (NFAState index) c
                            else 
                                if index == end_index' && c == Epsilon
                                    then single_nfa_end_state' : (single_nfa_start_state child' : (single_nfa_edges child' (NFAState index) c))
                                    else []
    _ -> error "Unexpected error"

regular_tokens_to_SingleNFA :: [REToken] -> SingleNFA
-- regular_tokens_to_SingleNFA' tokens s
-- @param tokens    regular tokens
-- @param s         output stack
regular_tokens_to_SingleNFA tokens = regular_tokens_to_SingleNFA' tokens [] where
    regular_tokens_to_SingleNFA' [] [single_nfa] = single_nfa
    regular_tokens_to_SingleNFA' [] _ = error "Regular expression not valid!"
    regular_tokens_to_SingleNFA' xs@(x:remain) s = case x of
        REChar c -> regular_tokens_to_SingleNFA' remain (char_to_SingleNFA c : s)
        REOperator And -> case s of
            (rchild : lchild : s_remain) -> regular_tokens_to_SingleNFA' remain $ binary_operator_to_SingleNFA And lchild rchild : s_remain
            _ -> error "Regular expression not valid!"
        REOperator Or -> case s of
            (rchild : lchild : s_remain) -> regular_tokens_to_SingleNFA' remain $ binary_operator_to_SingleNFA Or lchild rchild : s_remain
            _ -> error "Regular expression not valid!"
        REOperator Repeat -> case s of 
            (child : s_remain) -> regular_tokens_to_SingleNFA' remain $ unary_operator_to_SingleNFA Repeat child : s_remain
            _ -> error "Regular expression not valid!"
        _ -> error "Unexpected error"

data NFA = NFA { nfa_states :: [NFAState]
               , nfa_charset :: Set.Set RECharType
               , nfa_edges :: (NFAState -> RECharType -> [NFAState])
               , nfa_start_state :: NFAState
               , nfa_end_states :: [NFAState]
               , nfa_id :: (NFAState -> Maybe REID)
}

instance Show NFA where
    show nfa = states_str ++ "\n" ++ start_state_str ++ "\n" ++ end_state_str ++ "\n" ++ edges_str
        where
            nfa_states' = List.sort $ nfa_states nfa
            states_str = "States: " ++ (show nfa_states')
            start_state_str = "Start state: " ++ (show $ nfa_start_state nfa)
            end_state_str = "End state: " ++ (show $ List.map 
                                (\state -> ((case nfa_id nfa state of
                                                Just id -> (id, state)
                                                Nothing -> error "Unexpected error"
                                ), state)) $ nfa_end_states nfa)
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

single_nfa_to_nfa :: SingleNFA -> REID -> NFA
single_nfa_to_nfa single_nfa id = NFA { nfa_states = single_nfa_states single_nfa
                                      , nfa_charset = single_nfa_charset single_nfa
                                      , nfa_edges = single_nfa_edges single_nfa
                                      , nfa_start_state = single_nfa_start_state single_nfa
                                      , nfa_end_states = [single_nfa_end_state single_nfa]
                                      , nfa_id = (\state -> if state == single_nfa_end_state single_nfa
                                                                then Just id
                                                                else Nothing )
                                      }

shift_NFA :: Int -> NFA -> NFA
shift_NFA index nfa = 
    NFA { nfa_states = nfa_states'
        , nfa_charset = nfa_charset nfa
        , nfa_edges = nfa_edges'
        , nfa_start_state = nfa_start_state'
        , nfa_end_states = nfa_end_states'
        , nfa_id = nfa_id'
        }
    where
        nfa_start_state' = shift_state index $ nfa_start_state nfa
        nfa_end_states' = List.map (\state -> shift_state index state) $ nfa_end_states nfa
        nfa_states' = List.map (shift_state index) (nfa_states nfa)
        nfa_edges' = \state c -> List.map (shift_state index) $ nfa_edges nfa (shift_state (negate index) state) c
        nfa_id' = \state -> nfa_id nfa $ shift_state index state

merge_nfas :: NFA -> NFA -> NFA
merge_nfas nfa1 nfa2 = NFA { nfa_states = nfa_states'
                           , nfa_charset = nfa_charset'
                           , nfa_edges = nfa_edges'
                           , nfa_start_state = nfa_start_state'
                           , nfa_end_states = nfa_end_states'
                           , nfa_id = nfa_id'
                           }
    where
        nfa1' = shift_NFA 1 nfa1
        nfa1_end_index = List.length (nfa_states nfa1') + 1
        nfa2' = shift_NFA (nfa1_end_index + 1) nfa2
        nfa_start_state' = NFAState 0
        nfa_states' = nfa_start_state' : (nfa_states nfa1') ++ (nfa_states nfa2')
        nfa_charset' = Set.union (nfa_charset nfa1) (nfa_charset nfa2)
        nfa_edges' = (\state@(NFAState index) c ->
                if index == 0 && c == Epsilon
                    then [nfa_start_state nfa1, nfa_start_state nfa2]
                    else 
                        if index <= nfa1_end_index
                            then nfa_edges nfa1' state c
                            else nfa_edges nfa2' state c
            )
        nfa_end_states' = List.sort $ nfa_end_states nfa1' ++ nfa_end_states nfa2'
        nfa_id' = (\state -> case nfa_id nfa1' state of
                                Just id -> Just id
                                Nothing -> nfa_id nfa2' state)

regular_expressions_to_NFA :: [(REID, [REToken])] -> NFA
regular_expressions_to_NFA [(id, tokens)] = single_nfa_to_nfa (regular_tokens_to_SingleNFA tokens) id
regular_expressions_to_NFA ((id, tokens):xs) = merge_nfas nfa1 nfa2
    where
        nfa1 = regular_expressions_to_NFA xs
        nfa2 = single_nfa_to_nfa (regular_tokens_to_SingleNFA tokens) id