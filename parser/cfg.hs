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
, singleton_grammar
, update_symbols
, update_productions
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
                       }

singleton_grammar :: Symbol -> Grammar
singleton_grammar start_symbol' = Grammar { symbols = Set.empty
                                          , start_symbol = start_symbol'
                                          , productions = (\_ -> Set.empty)
                                          }

update_symbols :: Grammar -> Set.Set Symbol -> Set.Set Symbol -> Grammar
update_symbols grammar terminals nonterminals = grammar { symbols = Set.union terminals nonterminals }

get_symbol :: Grammar -> String -> Maybe Symbol
get_symbol grammar str =
    if string == "ε"
        then Just Epsilon
        else
            if Set.member (Terminal str) $ symbols grammar
                then Just $ Terminal str
                else
                    if Set.member (Nonterminal str) $ symbols grammar
                        then Just $ Nonterminal str
                        else Nothing

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

first :: Grammar -> Symbol -> Set.Set Symbol
first grammar symbol@(Symbol _ is_terminal) = symbols
    where
        next_productions = productions grammar symbol
        symbols' = Set.foldl (\symbols rhs -> first' grammar symbol rhs symbols) Set.empty next_collections
        symbols = if is_terminal
                    then Set.singleton symbol
                    else symbols'

first' :: Grammar -> Symbol -> RHS -> Set.Set Symbol -> Set.Set Symbol
first' _ _ [] symbols = symbols
first' grammar symbol (s:remain) symbols = symbols'
    where
        first_s = first grammar s
        symbols'' = Set.union first_s symbols
        symbols' = if Set.member Epsilon first_s
                    then first' grammar symbol remain symbols''
                    else symbols''

follow :: Grammar -> Symbol -> Set.Set Symbol
follow grammar symbol = symbols
    where
        symbols' = Set.singleton EOF

        indices = Set.map (\)

follow' :: Grammar -> Symbol -> Symbol -> Set.Set RHS -> Set.Set Symbol -> Set.Set Symbol
follow' grammar symbol symbol' rhss symbols = symbols'
    where
        rhs = Set.elemAt 0 rhss
        indices = List.elemIndices symbol rhs
        symbols'' = List.foldl (\symbols1 index ->
            if index + 1 < List.length rhs
                then
                    if Set.member Epsilon next_first
                        then follow grammar symbol'
                        else next_first
                        where
                            next_first = first grammar $ rhs !! (index + 1)
                else follow grammar symbol'
            )
        symbols' = if Set.null rhss
                    then symbols
                    else follow' grammar symbol symbol' (Set.deleteAt 0 rhss) (Set.union symbols symbols'')
                                    