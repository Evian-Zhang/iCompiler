module CFG
( Symbol (Terminal, Nonterminal, Epsilon, EOF)
, to_terminal_symbols
, to_nonterminal_symbols
, is_terminal
, RHS
, Grammar (Grammar)
, symbols
, start_symbol
, productions
, first
, singleton_grammar
, update_symbols
, get_nonterminals
, update_productions
, is_nullable
, update_first
) where

import qualified Data.Set as Set
import qualified Data.List as List

data Symbol = Terminal String | Nonterminal String | Epsilon | EOF deriving (Eq, Ord)

instance Show Symbol where
    show symbol = case symbol of 
                    Terminal content -> content
                    Nonterminal content -> content
                    Epsilon -> "ε"
                    EOF -> "$"

to_terminal_symbols :: [String] -> Set.Set Symbol
to_terminal_symbols strs = List.foldl (\symbols str -> Set.insert (Terminal str) symbols) Set.empty strs

to_nonterminal_symbols :: [String] -> Set.Set Symbol
to_nonterminal_symbols strs = List.foldl (\symbols str -> Set.insert (Nonterminal str) symbols) Set.empty strs

is_terminal :: Symbol -> Bool
is_terminal (Nonterminal _) = False
is_terminal _ = True

type RHS = [Symbol]

data Grammar = Grammar { symbols :: Set.Set Symbol
                       , start_symbol :: Symbol
                       , productions :: Symbol -> Set.Set RHS
                       , first :: [Symbol] -> Set.Set Symbol
                       }

singleton_grammar :: Symbol -> Grammar
singleton_grammar start_symbol' = Grammar { symbols = Set.empty
                                          , start_symbol = start_symbol'
                                          , productions = (\_ -> Set.empty)
                                          , first = (\_ -> Set.empty)
                                          }

update_symbols :: Grammar -> Set.Set Symbol -> Set.Set Symbol -> Grammar
update_symbols grammar terminals nonterminals = grammar { symbols = Set.union terminals nonterminals }

get_symbol :: Grammar -> String -> Maybe Symbol
get_symbol grammar str =
    if str == "ε"
        then Just Epsilon
        else
            if Set.member (Terminal str) $ symbols grammar
                then Just $ Terminal str
                else
                    if Set.member (Nonterminal str) $ symbols grammar
                        then Just $ Nonterminal str
                        else Nothing

get_nonterminals :: Grammar -> Set.Set Symbol
get_nonterminals grammar = Set.filter (\symbol -> case symbol of
                                            Nonterminal _ -> True
                                            _ -> False) $ symbols grammar

update_productions :: Grammar -> String -> [String] -> Grammar
update_productions grammar lhs_str rhs_strs = grammar { productions = productions' }
    where
        lhs' = case get_symbol grammar lhs_str of
                Just symbol -> symbol
                Nothing -> error $ "Not declared symbol name " ++ lhs_str
        rhs' = List.foldl (\symbols str ->
                case get_symbol grammar str of
                    Just symbol -> symbols ++ [symbol]
                    Nothing -> error $ "Not declared symbol name " ++ str)
                [] rhs_strs
        productions' = (\symbol -> if symbol == lhs'
                                    then Set.insert rhs' $ productions grammar symbol
                                    else productions grammar symbol)

is_nullable :: Grammar -> [Symbol] -> Bool
is_nullable grammar [] = True
is_nullable grammar [symbol] = Set.member [Epsilon] $ productions grammar symbol
is_nullable grammar (s:remain) = is_nullable grammar [s] && is_nullable grammar remain

update_first :: Grammar -> Grammar
update_first grammar = grammar { first = first' }
    where
        first1 = update_first_with_terminals $ first grammar
        first' = update_others grammar first1
            where
                update_others grammar first1 = first'
                    where
                        first2 = Set.foldl (\first'' symbol -> 
                            update_first_with_nonterminal grammar symbol first'') 
                            first1 $ get_nonterminals grammar
                        is_first_stable = compare_first (symbols grammar) first1 first2
                            where
                                compare_first symbols first1 first2 =
                                    if Set.null symbols
                                        then True
                                        else if first1 [Set.elemAt 0 symbols] == first2 [Set.elemAt 0 symbols]
                                            then compare_first (Set.deleteAt 0 symbols) first1 first2
                                            else False
                        first' = if is_first_stable then first2 else update_others grammar first2

update_first_with_terminals :: ([Symbol] -> Set.Set Symbol) -> [Symbol] -> Set.Set Symbol
update_first_with_terminals first = 
    \symbols -> case symbols of
                    [symbol@(Terminal _)] -> Set.singleton symbol
                    [Epsilon] -> Set.singleton Epsilon
                    [EOF] -> Set.singleton EOF
                    _ -> first symbols

update_first_with_nonterminal :: Grammar -> Symbol -> ([Symbol] -> Set.Set Symbol) -> [Symbol] -> Set.Set Symbol
update_first_with_nonterminal grammar symbol first = 
    \symbols -> case symbols of
                    [symbol] -> Set.foldl (\symbols' rhs -> 
                        Set.union symbols' $ update_first_with_production grammar rhs first) Set.empty $ productions grammar symbol
                    _ -> first symbols

update_first_with_production :: Grammar-> RHS -> ([Symbol] -> Set.Set Symbol) -> Set.Set Symbol
update_first_with_production _ [s] first = first [s]
update_first_with_production grammar (s:ss) first = 
    Set.union (first [s]) (if is_nullable grammar [head ss]
                            then update_first_with_production grammar ss first
                            else Set.empty)
                                    