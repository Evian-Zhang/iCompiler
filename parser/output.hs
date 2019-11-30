module Output
( output_action
, output_goto
)
where

import qualified Data.Set as Set
import qualified Data.List as List

import CFG
import DFA
import Parsing_Table

output_action :: Grammar -> DFA -> String
output_action grammar dfa = terminals_str ++ actions_str
    where
        terminals = (Set.toList $ get_terminals grammar) ++ [EOF]
        terminals_str = "," ++ (display_terminals terminals)
            where
                display_terminals [t] = show t
                display_terminals (t:ts) = show t ++ "," ++ (display_terminals ts)
        dfa_collections = List.sortOn (\(LRCollection index _) -> index) $ Set.toList $ collections dfa
        actions_str = List.foldl (\actions_str' collection -> 
            actions_str' ++ "\n" ++ display_collection_action grammar dfa collection) "" dfa_collections

display_collection_action :: Grammar -> DFA -> LRCollection -> String
display_collection_action grammar dfa collection@(LRCollection index _) = show index ++ "," ++ actions_str
    where
        terminals = (Set.toList $ get_terminals grammar) ++ [EOF]
        actions_str = display_actions terminals
            where
                display_actions [t] = display_action $ action grammar dfa collection t
                display_actions (t:ts) = (display_action $ action grammar dfa collection t) ++ "," ++ (display_actions ts)

display_action :: Action -> String
display_action action = case action of
    Shift (LRCollection index _ ) -> "S" ++ (show index)
    Reduce production -> "R" ++ (show production)
    Accept -> "acc"
    Reject -> ""

output_goto :: Grammar -> DFA -> String
output_goto grammar dfa = nonterminals_str ++ gotos_str
    where
        nonterminals = Set.toList $ get_nonterminals grammar
        nonterminals_str = "," ++ (display_nonterminals nonterminals)
            where
                display_nonterminals [n] = show n
                display_nonterminals (n:ns) = show n ++ "," ++ (display_nonterminals ns)
        dfa_collections = List.sortOn (\(LRCollection index _) -> index) $ Set.toList $ collections dfa
        gotos_str = List.foldl (\gotos_str' collection ->
            gotos_str' ++ "\n" ++ display_collection_goto grammar dfa collection) "" dfa_collections

display_collection_goto :: Grammar -> DFA -> LRCollection -> String
display_collection_goto grammar dfa collection@(LRCollection index _) = show index ++ "," ++ gotos_str
    where
        nonterminals = Set.toList $ get_nonterminals grammar
        gotos_str = display_gotos nonterminals
            where
                display_gotos [n] = display_goto $ goto dfa collection n
                display_gotos (n:ns) = (display_goto $ goto dfa collection n) ++ "," ++ (display_gotos ns)
        
display_goto :: Maybe LRCollection -> String
display_goto next = case next of
    Just (LRCollection index _) -> show index
    Nothing -> ""
