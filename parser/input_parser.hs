module Input_Parser ( build_grammar ) where

import CFG

build_grammar :: String -> Grammar
build_grammar content = grammar
    where
        contents = lines content
        (start_symbol_str, contents1) = if length contents > 0 && head contents == "start symbol"
                                            then (contents !! 1, drop 2 contents)
                                            else error "No start symbol declared"
        (terminal_strs, contents2) = if length contents1 > 0 && head contents1 == "terminals"
                                        then get_terminal_strs $ tail contents1
                                        else error "No terminals declared"
        (nonterminal_strs, contents3) = if length contents2 > 0 && head contents2 == "nonterminals"
                                            then get_nonterminal_strs $ tail contents2
                                            else error "No nonterminal declared"
        production_strs = if length contents3 > 0 && head contents3 == "productions"
                            then get_production_strs $ tail contents3
                            else error "No productions declared"
        grammar1 = singleton_grammar $ Symbol start_symbol_str False
        terminals = to_terminal_symbols terminal_strs
        nonterminals = to_nonterminal_symbols nonterminal_strs
        grammar2 = update_symbols grammar1 terminals nonterminals
        grammar = foldl (\grammar' (lhs_str, rhs_strs) -> update_productions grammar' lhs_str rhs_strs) grammar2 production_strs

get_terminal_strs :: [String] -> ([String], [String])
get_terminal_strs strs@(c:cs) = if c == "nonterminals"
                                    then ([], strs)
                                    else (c : symbols, remain)
    where
        (symbols, remain) = get_terminal_strs cs

get_nonterminal_strs :: [String] -> ([String], [String])
get_nonterminal_strs strs@(c:cs) = if c == "productions"
                                    then ([], strs)
                                    else (c : symbols, remain)
    where
        (symbols, remain) = get_nonterminal_strs cs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = reverse $ wordsWhen' p s [[]]
    where
        wordsWhen' _ [] res = res
        wordsWhen' p (s:remain) res@(x:y) = if p s 
            then wordsWhen' p remain ([]:res)
            else wordsWhen' p remain ((x ++ [s]) : y)

get_production_strs :: [String] -> [(String, [String])]
get_production_strs [] = []
get_production_strs (c:cs) = productions ++ (get_production_strs cs)
    where
        (lhs_str, rhs_str) = get_lhs_str c
            where
                get_lhs_str [] = error "Production doesn't have '->'"
                get_lhs_str (a:b:cs) = if [a, b] == "->"
                                        then ([], cs)
                                        else (a : lhs_str, remain)
                    where
                        (lhs_str, remain) = get_lhs_str (b:cs)
        rhs_strs = wordsWhen (== '|') rhs_str
        rhs_groups = map (\rhs_str -> wordsWhen (== '.') rhs_str) rhs_strs
        productions = map (\rhs_tokens -> (lhs_str, rhs_tokens)) rhs_groups
