module CFG
( Symbol (Symbol)
, to_terminal_symbols
, to_nonterminal_symbols
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

data Symbol = Symbol String Bool deriving (Eq, Ord)

instance Show Symbol where
    show (Symbol content _) = content

to_terminal_symbols :: [String] -> Set.Set Symbol
to_terminal_symbols strs = List.foldl (\symbols str -> Set.insert (Symbol str True) symbols) Set.empty strs

to_nonterminal_symbols :: [String] -> Set.Set Symbol
to_nonterminal_symbols strs = List.foldl (\symbols str -> Set.insert (Symbol str False) symbols) Set.empty strs

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
    if Set.member (Symbol str True) $ symbols grammar
        then Just $ Symbol str True
        else
            if Set.member (Symbol str False) $ symbols grammar
                then Just $ Symbol str False
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