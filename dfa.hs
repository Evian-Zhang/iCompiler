module DFA
( DFAState (DFAState)
, DFA (DFA)
, dfa_states
, dfa_charset
, dfa_edges
, dfa_start_state
, dfa_end_states
, nfa_to_dfa
) where

import qualified Data.Set as Set
import qualified Data.List as List
    
import Re
import NFA

-- epsilon_closure_of_nfa_states nfa nfa_states
-- @breif find the epsilon closure of set nfa_states
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

-- terminal_closure_of_nfa_states nfa c nfa_states
-- @brief find the terminal closure from set nfa_states with edge c
terminal_closure_of_nfa_states :: NFA -> RECharType -> (Set.Set NFAState) -> (Set.Set NFAState)
terminal_closure_of_nfa_states nfa c nfa_states = Set.foldl (\new_nfa_states nfa_state -> Set.union new_nfa_states $ terminal_closure_of_nfa_state nfa c nfa_state) Set.empty nfa_states

data DFAState = DFAState Int (Set.Set NFAState)

instance Show DFAState where
    show (DFAState index _) = show index

-- Two DFA states are equal iff their sets of nfa_states are equal
instance Eq DFAState where
    (==) (DFAState _ nfa_states1) (DFAState _ nfa_states2) = nfa_states1 == nfa_states2

instance Ord DFAState where
    compare (DFAState _ nfa_states1) (DFAState _ nfa_states2) = compare nfa_states1 nfa_states2

data DFA = DFA { dfa_states :: Set.Set DFAState
               , dfa_charset :: Set.Set RECharType
               , dfa_edges :: (DFAState -> RECharType -> Maybe DFAState)
               , dfa_start_state :: DFAState
               , dfa_end_states :: Set.Set DFAState
               }

instance Show DFA where
    show dfa = states_str ++ "\n" ++ start_state_str ++ "\n" ++ end_states_str ++ "\n" ++ edges_str
        where
            dfa_states' = List.sortOn (\(DFAState index _) -> index) $ Set.toList $ dfa_states dfa
            states_str = "States:\n" ++ (show_states dfa_states')
            start_state_str = "Start state: " ++ (show $ dfa_start_state dfa)
            end_states_str = "End states: " ++ (show $ List.sortOn (\(DFAState index _) -> index) $ Set.toList $ dfa_end_states dfa)
            dfa_charset' = Set.toList $ dfa_charset dfa
            show_states [] = ""
            show_states ((DFAState index nfa_states):remain) = "DFAState " ++ (show index) ++ ":\n" ++ (show $ Set.toList nfa_states) ++ "\n" ++ (show_states remain)
            edges_str = show_state_edges dfa_states' dfa_charset' (dfa_edges dfa)
                where
                    show_state_edges dfa_states dfa_charset dfa_edges = if List.null dfa_states
                        then ""
                        else "\n" ++ (show_state_char_edges (head dfa_states') dfa_charset dfa_edges) ++ (show_state_edges (tail dfa_states) dfa_charset dfa_edges)
                            where
                                show_state_char_edges _ [] _ = ""
                                show_state_char_edges dfa_state charset dfa_edges = case dfa_edges dfa_state (head charset) of
                                    Nothing -> ""
                                    Just next_state -> show dfa_state ++ " -" ++ (show $ head charset) ++ "-> " ++ (show next_state) ++ "\n" ++ show_state_char_edges dfa_state (tail charset) dfa_edges


-- constructor of DFA with a single DFAState and a given charset
single_dfa :: DFAState -> (Set.Set RECharType) -> DFA
single_dfa dfa_state charset = DFA { dfa_states = Set.singleton dfa_state
                                   , dfa_charset = charset
                                   , dfa_edges = (\_ _ -> Nothing)
                                   , dfa_start_state = dfa_state
                                   , dfa_end_states = Set.empty
                                   }

-- convert NFA to DFA
nfa_to_dfa :: NFA -> DFA
nfa_to_dfa nfa = nfa_to_dfa' nfa [initial_dfa_state] $ single_dfa initial_dfa_state $ nfa_charset nfa
    where
        initial_dfa_state = DFAState 0 $ epsilon_closure_of_nfa_states nfa [nfa_start_state nfa]

-- nfa_to_dfa' nfa processing dfa
-- @brief convert NFA to DFA by generate and traverse all its DFA state
-- @param nfa           the NFA to be converted from
-- @param processing    a set of DFAState to be processed
-- @param dfa           current DFA
nfa_to_dfa' :: NFA -> [DFAState] -> DFA -> DFA
nfa_to_dfa' nfa [] dfa = dfa
nfa_to_dfa' nfa processing dfa = nfa_to_dfa' nfa processing' dfa'
    where
        dfa_state = head processing
        dfa' = nfa_to_dfa'' nfa (Set.toList (nfa_charset nfa)) dfa_state dfa
        new_dfa_states = Set.toList (dfa_states dfa' Set.\\ (dfa_states dfa))
        new_dfa_states' = List.sortOn (\(DFAState index _) -> index) new_dfa_states
        processing' = tail processing ++ new_dfa_states'

-- nfa_to_dfa'' nfa nfa_charset nfa_state dfa
-- @brief for a given NFA and DFA state, find its all successors via a given charset
-- @param nfa           the NFA to be converted
-- @param nfa_charset   remaining charset of NFA
-- @param dfa_state     the DFA state to generate others
-- @param dfa           the DFA to be updated
nfa_to_dfa'' :: NFA -> [RECharType] -> DFAState -> DFA -> DFA
nfa_to_dfa'' nfa [] dfa_state dfa = dfa
nfa_to_dfa'' nfa nfa_charset dfa_state dfa = nfa_to_dfa'' nfa (tail nfa_charset) dfa_state dfa'
        where
            c = head nfa_charset
            DFAState index nfa_states = dfa_state
            encounted = dfa_states dfa
            next_nfa_states = epsilon_closure_of_nfa_states nfa $ Set.toList $ terminal_closure_of_nfa_states nfa c nfa_states
            next_dfa_state = DFAState (Set.size encounted) next_nfa_states
            (next_dfa_state', encounted') = case Set.lookupIndex next_dfa_state encounted of
                                                Just index' -> (Set.elemAt index' encounted, encounted)
                                                Nothing -> (next_dfa_state, Set.insert next_dfa_state encounted)
            dfa_edges' = if not $ Set.null next_nfa_states
                            then
                                (\the_state@(DFAState the_index _) the_c ->
                                    if the_index == index && the_c == c
                                        then Just next_dfa_state'
                                        else dfa_edges dfa the_state c
                                )
                            else
                                dfa_edges dfa
            dfa_end_states' = if not $ Set.null $ Set.filter (\nfa_state -> nfa_end_state nfa == nfa_state) next_nfa_states
                                then Set.insert next_dfa_state' $ dfa_end_states dfa
                                else dfa_end_states dfa
            dfa' = if not (Set.null next_nfa_states)
                    then
                        DFA { dfa_states = encounted'
                            , dfa_charset = dfa_charset dfa
                            , dfa_edges = dfa_edges'
                            , dfa_start_state = dfa_start_state dfa
                            , dfa_end_states = dfa_end_states'
                            }
                    else
                        dfa