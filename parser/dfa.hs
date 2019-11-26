module DFA where

import CFG

import qualified Data.Set as Set
import qualified Data.List as List

augment_grammar :: Grammar -> Grammar
augment_grammar grammar = Grammar { symbols = symbols'
                                  , start_symbol = start_symbol'
                                  , productions = productions'
                                  }
    where
        Symbol start_symbol_content _ = start_symbol grammar
        start_symbol' = Symbol (start_symbol_content ++ "'") False
        symbols' = Set.insert start_symbol' $ symbols grammar
        productions' = (\symbol -> if symbol == start_symbol'
                                    then Set.singleton [start_symbol grammar]
                                    else productions grammar symbol)

data LRItem = LRItem Symbol RHS Int deriving (Eq, Ord)

instance Show LRItem where
    show (LRItem lhs rhs index) = show rhs ++ "->" ++ (show_rhs rhs index)
        where
            show_rhs_list rhs = List.foldl (\str symbol -> str ++ (show symbol)) [] rhs
            show_rhs rhs index = show_rhs_list (take index rhs) ++ "." ++ (show_rhs_list $ drop index rhs)

init_item :: Symbol -> RHS -> LRItem
init_item lhs rhs = if rhs == [Symbol "ε" True]
                        then LRItem lhs [] 0
                        else LRItem lhs rhs 0

is_reducible :: LRItem -> Bool
is_reducible (LRItem _ rhs index) = List.length rhs == index

one_level_closure_item :: Grammar -> LRItem -> Set.Set LRItem
one_level_closure_item grammar item@(LRItem lhs rhs index) = items
    where
        current_symbol = rhs !! index
        rhss = productions grammar current_symbol
        items' = if is_reducible item || Set.null rhss
                    then Set.empty
                    else Set.map (\rhs' -> init_item current_symbol rhs') rhss
        items = Set.insert item items'

closure_items :: Grammar -> Set.Set LRItem -> Set.Set LRItem
closure_items grammar items = items'
    where
        new_closure = Set.foldl (\items'' item -> Set.union items'' $ one_level_closure_item grammar item) Set.empty items
        added_items = new_closure Set.\\ items
        items' = if Set.null items
                    then Set.empty
                    else
                        Set.union new_closure $ closure_items grammar added_items

goto_items :: Grammar -> Set.Set LRItem -> Symbol -> Set.Set LRItem
goto_items grammar items s = items'
    where
        shiftable_items = Set.filter (\(LRItem _ rhs index) -> index < length rhs && rhs !! index == s) items
        shift_items = Set.map (\(LRItem lhs rhs index) -> LRItem lhs rhs (index + 1)) shiftable_items
        items' = closure_items grammar shift_items

data LRCollection = LRCollection Int (Set.Set LRItem) deriving (Show)

instance Eq LRCollection where
    (==) (LRCollection _ items1) (LRCollection _ items2) = items1 == items2

instance Ord LRCollection where
    compare (LRCollection _ items1) (LRCollection _ items2) = compare items1 items2

data DFA = DFA { collections :: Set.Set LRCollection
               , start_collection :: LRCollection
               , goto :: LRCollection -> Symbol -> Maybe LRCollection
               }

grammar_to_DFA :: Grammar -> DFA
grammar_to_DFA grammar = dfa
    where
        start_symbol' = start_symbol grammar
        start_item = LRItem start_symbol' (Set.elemAt 0 $ productions grammar start_symbol') 0
        start_collection' = LRCollection 0 $ closure_items grammar $ Set.singleton start_item
        dfa' = DFA { collections = Set.singleton start_collection'
                   , start_collection = start_collection'
                   , goto = (\_ _ -> Nothing)
                   }
        dfa = grammar_to_DFA' grammar [start_collection'] dfa'

grammar_to_DFA' :: Grammar -> [LRCollection] -> DFA -> DFA
grammar_to_DFA' _ [] dfa = dfa
grammar_to_DFA' grammar (collection:remain) dfa = grammar_to_DFA' grammar remain' dfa'
    where
        dfa' = grammar_to_DFA'' grammar collection (Set.toList $ symbols grammar) dfa
        added_collections = collections dfa' Set.\\ (collections dfa)
        added_collections' = Set.toList added_collections
        added_collections'' = List.sortOn (\(LRCollection index _) -> index) added_collections'
        remain' = remain ++ added_collections''

grammar_to_DFA'' :: Grammar -> LRCollection -> [Symbol] -> DFA -> DFA
grammar_to_DFA'' _ _ [] dfa = dfa
grammar_to_DFA'' grammar collection@(LRCollection _ items) (s:remain) dfa = grammar_to_DFA'' grammar collection remain dfa'
    where
        new_items = goto_items grammar items s
        new_collection = LRCollection (Set.size $ collections dfa) new_items
        new_collection' = case Set.lookupIndex new_collection $ collections dfa of
                            Just index -> Set.elemAt index $ collections dfa
                            Nothing -> new_collection
        collections' = Set.insert new_collection' $ collections dfa
        goto' = (\collection' symbol -> 
                    if collection' == collection && symbol == s
                        then Just new_collection'
                        else goto dfa collection s)
        dfa' = if Set.null new_items
                then dfa
                else dfa { collections = collections'
                         , goto = goto'
                         }
