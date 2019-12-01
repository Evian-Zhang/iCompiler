module Input_Parser
( parse_action_goto
, to_symbol
, wordsWhen
) where

import qualified Data.Set as Set
import qualified Data.List as List

import Core

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = reverse $ wordsWhen' p s [[]]
    where
        wordsWhen' _ [] res = res
        wordsWhen' p (s:remain) res@(x:y) = if p s 
            then wordsWhen' p remain ([]:res)
            else wordsWhen' p remain ((x ++ [s]) : y)

to_symbol :: String -> Symbol
to_symbol "ε" = Epsilon
to_symbol "$" = EOF
to_symbol symbol_str = Normal symbol_str

parse_action_goto :: String -> String -> DFA
parse_action_goto action_str goto_str = dfa
    where
        dfa1 = singleton_dfa
        action_strs = lines action_str
        terminals_str = head action_strs
        terminals_strs = wordsWhen (== ',') terminals_str
        terminals = List.map to_symbol $ tail terminals_strs
        dfa2 = List.foldl (\dfa' action_str -> update_action dfa' terminals action_str) dfa1 $ tail action_strs
        goto_strs = lines goto_str
        nonterminals_str = head goto_strs
        nonterminals_strs = wordsWhen (== ',') nonterminals_str
        nonterminals = List.map to_symbol $ tail nonterminals_strs
        dfa = List.foldl (\dfa' goto_str -> update_goto dfa' nonterminals goto_str) dfa2 $ tail goto_strs

to_production :: String -> Production
to_production production_str = Production lhs rhs
    where
        (lhs_str, rhs_str) = get_lhs_str production_str
            where
                get_lhs_str [] = error "Production doesn't have '->'"
                get_lhs_str (a:b:cs) = if [a, b] == "->"
                                        then ([], cs)
                                        else (a : lhs_str, remain)
                    where
                        (lhs_str, remain) = get_lhs_str (b:cs)
        lhs = to_symbol lhs_str
        rhs = List.map to_symbol $ wordsWhen (== '.') rhs_str

to_action :: String -> Action
to_action [] = Reject
to_action (c:cs) = case c of
    'S' -> Shift $ read cs
    'R' -> Reduce $ to_production cs
    'a' -> Accept
    _ -> Reject

update_action :: DFA -> [Symbol] -> String -> DFA
update_action dfa terminals action_str = update_action' dfa terminals $ wordsWhen (== ',') action_str
    where
        update_action' dfa terminals (index_str:remain) = dfa { collections = collections'
                                                              , action = action'
                                                              }
            where
                index = read index_str
                collections' = Set.insert index $ collections dfa
                tokens = zip [0..] remain
                action' = List.foldl (\action'' (index', token) -> 
                    \the_index the_symbol ->
                        if the_index == index && the_symbol == terminals !! index'
                            then to_action token
                            else action'' the_index the_symbol) (action dfa) tokens

to_goto :: String -> Maybe Int
to_goto [] = Nothing
to_goto str = Just $ read str

update_goto :: DFA -> [Symbol] -> String -> DFA
update_goto dfa nonterminals goto_str = update_goto' dfa nonterminals $ wordsWhen (== ',') goto_str
    where
        update_goto' dfa nonterminals (index_str:remain) = dfa { collections = collections'
                                                               , goto = goto'
                                                               }
            where
                index = read index_str
                collections' = Set.insert index $ collections dfa
                tokens = zip [0..] remain
                goto' = List.foldl (\goto'' (index', token) ->
                    \the_index the_symbol ->
                        if the_index == index && the_symbol == nonterminals !! index'
                            then to_goto token
                            else goto'' the_index the_symbol) (goto dfa) tokens