module DFA
( DFAState
, DFA
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

data DFAState = DFAState Int (Set.Set NFAState) deriving (Show)

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

single_dfa :: DFAState -> (Set.Set RECharType) -> DFA
single_dfa dfa_state charset = DFA { dfa_states = Set.singleton dfa_state
                           , dfa_charset = charset
                           , dfa_edges = (\_ _ -> Nothing)
                           , dfa_start_state = dfa_state
                           , dfa_end_states = Set.empty
                           }

nfa_to_dfa :: NFA -> DFA
nfa_to_dfa nfa = nfa_to_dfa' nfa [initial_dfa_state] $ single_dfa initial_dfa_state $ nfa_charset nfa
    where
        initial_dfa_state = DFAState 0 $ epsilon_closure_of_nfa_states nfa [nfa_start_state nfa]

-- nfa_to_dfa' nfa processing dfa
-- @param nfa the NFA to be converted from
-- @param processing a set of DFAState to be processed
-- @param dfa current DFA
nfa_to_dfa' :: NFA -> [DFAState] -> DFA -> DFA
nfa_to_dfa' nfa [] dfa = dfa
nfa_to_dfa' nfa processing dfa = nfa_to_dfa' nfa processing' dfa'
    where
        dfa_state = head processing
        dfa' = nfa_to_dfa'' nfa (Set.toList (nfa_charset nfa)) dfa_state dfa
        new_dfa_states = Set.toList (dfa_states dfa' Set.\\ (dfa_states dfa))
        new_dfa_states' = List.sortOn (\(DFAState index _) -> index) new_dfa_states
        processing' = tail processing ++ new_dfa_states'

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
            dfa_edges' = if not (Set.null next_nfa_states)
                            then
                                (\the_state@(DFAState the_index _) the_c ->
                                    if the_index == index && the_c == c
                                        then Just next_dfa_state'
                                        else dfa_edges dfa the_state c
                                )
                            else
                                dfa_edges dfa
            dfa_end_states' = if not $ List.null $ List.filter (\nfa_state -> nfa_end_state nfa == nfa_state) $ Set.toList next_nfa_states
                                then Set.insert next_dfa_state $ dfa_end_states dfa
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