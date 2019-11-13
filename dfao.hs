module DFAO
( DFAOState (DFAOState)
, DFAO (DFAO)
, dfao_states
, dfao_start_state
, dfao_charset
, dfao_end_states
, dfao_edges
, dfa_to_dfao
) where

import qualified Data.Set as Set
import qualified Data.List as List

import Re
import DFA

-- find the DFA state with smallest index in a given set
find_smallest_index :: [DFAState] -> DFAState
find_smallest_index [dfa_state] = dfa_state
find_smallest_index dfa_states = if index < min_index then dfa_state else min_state
    where
        dfa_state@(DFAState index _) = head dfa_states
        min_state@(DFAState min_index _) = find_smallest_index $ tail dfa_states

-- merge the set of DFA state into one dfa state which has the smallest index in the set
merge_dfa_states :: DFA -> [DFAState] -> DFA
merge_dfa_states dfa dfa_states = merge_dfa_states' dfa dfa_state dfa_states
        where
            dfa_state = find_smallest_index dfa_states
            merge_dfa_states' dfa _ [] = dfa
            merge_dfa_states' dfa dfa_state dfa_states = merge_dfa_states' dfa' dfa_state $ tail dfa_states
                where
                    to_merge = head dfa_states
                    dfa' = merge_dfa_state dfa dfa_state to_merge

-- merge_dfa_state dfa dfa_state to_merge
-- @brief merge a DFA state into another DFA state
-- @param DFA           the DFA to be updated
-- @param dfa_state     the DFA state to be merged to
-- @param to_merge      the DFA state to be merged
merge_dfa_state :: DFA -> DFAState -> DFAState -> DFA
merge_dfa_state dfa dfa_state to_merge = dfa'
    where
        DFAState merged_index _ = dfa_state
        DFAState to_merge_index _ = to_merge
        dfa_states' = Set.delete to_merge $ dfa_states dfa
        dfa_end_states' = Set.delete to_merge $ dfa_end_states dfa
        dfa_edges' = \the_state@(DFAState index _) c -> 
            if index == to_merge_index
                then Nothing
                else case dfa_edges dfa the_state c of
                        Just out_state@(DFAState out_index _) -> 
                            if out_index == to_merge_index then Just dfa_state else Just out_state
                        Nothing -> Nothing
        dfa' = if dfa_state == to_merge
                then dfa
                else
                    DFA { dfa_states = dfa_states'
                        , dfa_charset = dfa_charset dfa
                        , dfa_edges = dfa_edges'
                        , dfa_start_state = dfa_start_state dfa
                        , dfa_end_states = dfa_end_states'
                        }

-- split_dfa dfa
-- @brief split a given DFA into two sets of DFA states, corresponding to whether it is an accpeting state
-- @param dfa                   the DFA to be split
-- @return (non_accpetings, acceptings)
--          non_accpetings:     the set of DFA states which is not an accpeting state
--          acceptings:         the set of DFA states which is an accepting state
split_dfa :: DFA -> ([DFAState], [DFAState])
split_dfa dfa = Set.foldl (\(non_acceptings, acceptings) dfa_state ->
                            if Set.member dfa_state $ dfa_end_states dfa
                                then (non_acceptings, dfa_state : acceptings)
                                else (dfa_state : non_acceptings, acceptings)) ([], []) (dfa_states dfa)

-- hopcroft dfa p w
-- @brief Hopcroft algorthim to minimize DFA
-- @brief p     current set of equivelent classes
-- @brief w     set of remaining groups that haven't been divided
hopcroft :: DFA -> Set.Set (Set.Set DFAState) -> Set.Set (Set.Set DFAState) -> Set.Set (Set.Set DFAState)
hopcroft dfa p w = if Set.null w then p else hopcroft dfa p' w'
    where
        a = Set.elemAt 0 w
        (w', p') = hopcroft_for_c dfa (Set.toList $ dfa_charset dfa) a (Set.deleteAt 0 w, p)

-- hopcroft_for_c dfa cs a (w, p)
-- @brief auxiliary function for hopcroft. Used to travese charset
-- @param dfa       current DFA
-- @param cs        remaining charset
-- @param a         current set of DFA states that is being processed
-- @param (w, p)    current (w, p)
hopcroft_for_c :: DFA -> [RECharType] -> (Set.Set DFAState) -> (Set.Set (Set.Set DFAState), Set.Set (Set.Set DFAState)) -> (Set.Set (Set.Set DFAState), Set.Set (Set.Set DFAState))
hopcroft_for_c dfa [] _ (w, p) = (w, p)
hopcroft_for_c dfa (c:cs) a (w, p) = hopcroft_for_c dfa cs a (w', p')
    where
        x = Set.filter (\dfa_state -> case dfa_edges dfa dfa_state c of
                                        Just x -> Set.member x a
                                        Nothing -> False) $ dfa_states dfa
        ys = Set.filter (\dfa_states -> (not $ Set.null $ Set.intersection x dfa_states) && (not $ Set.null $ dfa_states Set.\\ x)) p
        (w', p') = hopcroft_for_y dfa x ys (w, p)

-- hopcroft_for_y dfa x ys (w, p)
-- @brief auxiliary function for hopcroft. Used to traverse equivalent classes
-- @param dfa       current DFA
-- @param x         a set of DFA states used in the algorithm
-- @param ys        a set of equivalent classes used in the algorithm
-- @param (w, p)    current (w, p)
hopcroft_for_y :: DFA -> Set.Set DFAState -> Set.Set (Set.Set DFAState) -> (Set.Set (Set.Set DFAState), Set.Set (Set.Set DFAState)) -> (Set.Set (Set.Set DFAState), Set.Set (Set.Set DFAState))
hopcroft_for_y dfa x ys (w, p) = if Set.null ys then (w, p) else hopcroft_for_y dfa x ys' (w', p')
    where
        y = Set.elemAt 0 ys
        ys' = Set.deleteAt 0 ys
        xny = Set.intersection x y
        yx = y Set.\\ x
        p' = Set.insert yx $ Set.insert xny $ Set.delete y p
        w' = if Set.member y w
                then Set.insert yx $ Set.insert xny $ Set.delete y w
                else
                    if Set.size xny < Set.size yx
                        then Set.insert xny $ Set.delete y w
                        else Set.insert yx $ Set.delete y w

-- reduce states in DFA according to the equivalent class
reduce_dfa_states :: DFA -> [Set.Set DFAState] -> DFA
reduce_dfa_states dfa [] = dfa
reduce_dfa_states dfa (dfa_states:sets) = reduce_dfa_states dfa' sets
    where
        dfa' = merge_dfa_states dfa (Set.toList dfa_states)

data DFAOState = DFAOState Int deriving (Eq, Ord)

instance Show DFAOState where
    show (DFAOState index) = show index

data DFAO = DFAO { dfao_states :: Set.Set DFAOState
                 , dfao_charset :: Set.Set RECharType
                 , dfao_edges :: (DFAOState -> RECharType -> (Maybe DFAOState))
                 , dfao_start_state :: DFAOState
                 , dfao_end_states :: Set.Set DFAOState
                 }

instance Show DFAO where
    show dfao = states_str ++ "\n" ++ start_state_str ++ "\n" ++ end_states_str ++ "\n" ++ edges_str
        where
            dfao_states' = Set.toList $  dfao_states dfao
            states_str = "States:\n" ++ (show dfao_states')
            start_state_str = "Start state: " ++ (show $ dfao_start_state dfao)
            end_states_str = "End states: " ++ (show $ Set.toList $ dfao_end_states dfao)
            dfao_charset' = Set.toList $ dfao_charset dfao
            edges_str = show_state_edges dfao_states' dfao_charset' (dfao_edges dfao)
                where
                    show_state_edges dfao_states dfao_charset dfao_edges = if List.null dfao_states
                        then ""
                        else show_state_char_edges (head dfao_states) dfao_charset dfao_edges ++ (show_state_edges (tail dfao_states) dfao_charset dfao_edges)
                            where
                                show_state_char_edges _ [] _ = ""
                                show_state_char_edges dfao_state charset dfao_edges = (case dfao_edges dfao_state (head charset) of
                                    Nothing -> ""
                                    Just next_state -> "\n" ++ (show dfao_state) ++ " -" ++ (show $ head charset) ++ "-> " ++ (show next_state)) ++ show_state_char_edges dfao_state (tail charset) dfao_edges

-- @brief renumber the states in DFA
-- @discuss for example, the reduced dfa has states: 0, 2, 4, 5. Then, renumber it to 0, 1, 2, 3
-- @return (index_map, reverse_index_map)
--          index_map           from initial state index to renumbered index
--          reverse_index_map   from renumbered index to initial state index
get_index_map :: (Set.Set DFAState) -> (Int -> Maybe Int, Int -> Maybe Int)
get_index_map dfa_states = get_index_map' dfa_states' indices
    where
        dfa_states' = List.sortOn (\(DFAState index _) -> index) $ Set.toList dfa_states
        indices = [0, 1 ..]
        get_index_map' [] _ = ((\_ -> Nothing), (\_ -> Nothing))
        get_index_map' ((DFAState state_index _):remain_states) (index:remain_indices) =
            ((\x -> if x == state_index
                        then Just index
                        else fst (get_index_map' remain_states remain_indices) x),
            (\x -> if x == index
                    then Just state_index
                    else snd (get_index_map' remain_states remain_indices) x))

-- from DFA to optimized DFA
dfa_to_dfao :: DFA -> DFAO
dfa_to_dfao dfa = DFAO { dfao_states = dfao_states'
                       , dfao_charset = dfa_charset dfa
                       , dfao_edges = dfao_edges'
                       , dfao_start_state = dfao_start_state'
                       , dfao_end_states = dfao_end_states'
                       }
    where
        (non_acceptings, acceptings) = split_dfa dfa
        equivalent_classes = Set.fromList [Set.fromList non_acceptings, Set.fromList acceptings]
        equivalent_classes' = hopcroft dfa equivalent_classes equivalent_classes
        dfa' = reduce_dfa_states dfa $ Set.toList equivalent_classes'
        (index_map, reverse_index_map) = get_index_map $ dfa_states dfa'
        dfao_states' = Set.map (\(DFAState index _) -> case index_map index of
                                    Just a -> DFAOState a
                                    Nothing -> error "Unexpected error") $ dfa_states dfa'
        dfao_edges' = (\(DFAOState index) c -> 
            case reverse_index_map index of
                Just a -> case dfa_edges dfa' (DFAState a Set.empty) c of
                            Just (DFAState x _) -> case index_map x of 
                                        Just y -> Just (DFAOState y)
                                        Nothing -> error "Unexpected error"
                            Nothing -> Nothing
                Nothing -> Nothing
            )
        DFAState start_index _ = dfa_start_state dfa'
        dfao_start_state' = DFAOState (case index_map start_index of
                                        Just x -> x
                                        Nothing -> error "Unexpected error")
        dfao_end_states' = Set.map (\(DFAState index _) -> 
            DFAOState (case index_map index of
                        Just x -> x
                        Nothing -> error "Unexpected error")) $ dfa_end_states dfa'
