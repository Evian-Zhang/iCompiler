module Stacker ( construct_parsetree ) where

import Core
import Input_Parser

import qualified Data.Set as Set
import qualified Data.List as List

construct_parsetree :: DFA -> String -> ParseTree
construct_parsetree dfa string = construct_parsetree' dfa symbols [0] []
    where
        symbols = (List.map to_symbol $ wordsWhen (== '.') string) ++ [EOF]

show_symbols :: [Symbol] -> String
show_symbols symbols = List.concatMap (\symbol -> show symbol) symbols

construct_parsetree' :: DFA -> [Symbol] -> [Int] -> [ParseTree] -> ParseTree
construct_parsetree' dfa symbols collections_stack symbols_stack = node
    where
        the_symbol = head symbols
        the_action = action dfa (head collections_stack) the_symbol
        node = case the_action of
            Shift next_collection -> construct_parsetree' dfa (tail symbols) (next_collection : collections_stack) ((ParseTree the_symbol []) : symbols_stack)
            Reduce (Production lhs rhs) -> construct_parsetree' dfa symbols collections_stack' symbols_stack'
                where
                    poped_count = case rhs of
                                    [Epsilon] -> 0
                                    _ -> List.length rhs
                    (poped_symbols, remain_symbols) = splitAt poped_count symbols_stack
                    new_node = case rhs of
                                    [Epsilon] -> ParseTree lhs [ParseTree Epsilon []]
                                    _ -> ParseTree lhs $ List.foldl (\rhs' symbol -> symbol : rhs') [] poped_symbols
                    symbols_stack' = new_node : remain_symbols
                    remain_collections = drop poped_count collections_stack
                    current_top = head remain_collections
                    collections_stack' = (case goto dfa current_top lhs of
                        Just index -> index
                        Nothing -> error $ "Parsing error since " ++ (take 10 $ show_symbols symbols)) : remain_collections
            Accept -> head symbols_stack
            Reject -> error $ "Parsing error since " ++ (take 10 $ show_symbols symbols)
