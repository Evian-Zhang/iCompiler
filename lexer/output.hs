module Output
( display_dfao_edges
, display_dfao_end_states
) where

import qualified Data.Set as Set    

import Re
import DFAO

display_dfao_edges :: DFAO -> String
display_dfao_edges dfao = "," ++ (display_dfao_charset $ Set.toList $ dfao_charset dfao) ++ "\n" ++ (display_dfao_edges' (Set.toList $ dfao_states dfao) (Set.toList $ dfao_charset dfao) (dfao_edges dfao))
    where
        display_dfao_charset [c] = show c
        display_dfao_charset (c:cs) = show c ++ (',' : (display_dfao_charset cs))
        display_dfao_edges' [dfao_state] charset edges = show dfao_state ++ "," ++ (display_dfao_state dfao_state charset edges)
        display_dfao_edges' (dfao_state:remain) charset edges = show dfao_state ++ "," ++ (display_dfao_state dfao_state charset edges) ++ "\n" ++ (display_dfao_edges' remain charset edges)
        display_dfao_state dfao_state [c] edges = case edges dfao_state c of
                                                    Just next_state -> show next_state
                                                    Nothing -> ""
        display_dfao_state dfao_state (c:cs) edges = (case edges dfao_state c of
                                                        Just next_state -> show next_state
                                                        Nothing -> "")
                                                        ++ "," ++ display_dfao_state dfao_state cs edges

display_dfao_end_states :: DFAO -> String
display_dfao_end_states dfao = display_dfao_end_states' (Set.toList $ dfao_end_states dfao) (dfao_id dfao)
    where
        display_dfao_end_states' [dfao_state] id = show dfao_state ++ "," ++ (case id dfao_state of
                                                                        Just x -> show x
                                                                        Nothing -> error "Unexpected error")
        display_dfao_end_states' (dfao_state:remain) id = show dfao_state ++ "," ++ (case id dfao_state of
            Just x -> show x
            Nothing -> error "Unexpected error") ++ "\n" ++ (display_dfao_end_states' remain id)