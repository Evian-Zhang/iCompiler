module DFA
( LRItem (LRItem)
, item_lhs
, item_rhs
, item_dot
, item_lookaheads
, is_reducible
, LRCollection (LRCollection)
, DFA (DFA)
, collections
, dfa_symbols
, start_collection
, goto
, grammar_to_DFA
)
where

import CFG

import qualified Data.Set as Set
import qualified Data.List as List

augment_grammar :: Grammar -> Grammar
augment_grammar grammar = grammar { symbols = symbols'
                                  , start_symbol = start_symbol'
                                  , productions = productions'
                                  }
    where
        start_symbol_content = case start_symbol grammar of
                                Nonterminal content -> content
                                _ -> error "Unexpected error"
        start_symbol' = Nonterminal (start_symbol_content ++ "'")
        symbols' = Set.insert start_symbol' $ symbols grammar
        productions' = (\symbol -> if symbol == start_symbol'
                                    then Set.singleton [start_symbol grammar]
                                    else productions grammar symbol)

data LRItem = LRItem { item_lhs :: Symbol
                     , item_rhs :: RHS
                     , item_dot :: Int
                     , item_lookaheads :: Set.Set Symbol
                     }

instance Eq LRItem where
    (==) item1 item2 = item1 == item2'
        where
            item2' = item2 { item_lookaheads = item_lookaheads item1 }

instance Ord LRItem where
    compare item1 item2 = compare item1 item2'
        where
            item2' = item2 { item_lookaheads = item_lookaheads item1 }

instance Show LRItem where
    show item = (show $ item_lhs item) ++ "->" ++ (show_rhs (item_rhs item) (item_dot item)) ++ "\t" ++ (show $ Set.toList $ item_lookaheads item)
        where
            show_rhs_list rhs = List.foldl (\str symbol -> str ++ (show symbol)) [] rhs
            show_rhs rhs index = show_rhs_list (take index rhs) ++ "." ++ (show_rhs_list $ drop index rhs)

init_item :: Symbol -> RHS -> Set.Set Symbol -> LRItem
init_item lhs rhs lookaheads = LRItem { item_lhs = lhs
                                      , item_rhs = rhs
                                      , item_dot = 0
                                      , item_lookaheads = lookaheads
                                      }

merge_item :: LRItem -> LRItem -> LRItem
merge_item item1 item2 = item
    where
        new_lookaheads = Set.union (item_lookaheads item1) (item_lookaheads item2)
        item = item2 { item_lookaheads = new_lookaheads }

is_reducible :: LRItem -> Bool
is_reducible item = (List.length $ item_rhs item) == item_dot item

one_level_closure_item :: Grammar -> LRItem -> Set.Set LRItem
one_level_closure_item grammar item = items
    where
        current_symbol = (item_rhs item) !! (item_dot item)
        rhss = productions grammar current_symbol
        items' = if is_reducible item || Set.null rhss
                    then Set.empty
                    else Set.foldl (\items'' rhs -> 
                        update_closure items'' $ 
                            init_item current_symbol rhs $ 
                                Set.foldl (\lookaheads lookahead -> Set.union lookaheads $ 
                                    first grammar (following_symbols ++ [lookahead])) Set.empty $ item_lookaheads item)
                                Set.empty rhss
            where
                following_symbols = drop (item_dot item + 1) $ item_rhs item
                update_closure items item = items'
                    where
                        index' = Set.lookupIndex item items
                        items' = case index' of
                            Just index'' -> Set.insert (merge_item item' item) items
                                where
                                    item' = Set.elemAt index'' items
                            Nothing -> Set.insert item items
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
        shiftable_items = Set.filter (\item -> not (is_reducible item) && ((item_rhs item) !! (item_dot item) == s)) items
        shift_items = Set.map (\item' -> item' { item_dot = item_dot item' + 1 }) shiftable_items
        items' = closure_items grammar shift_items

data LRCollection = LRCollection Int (Set.Set LRItem) deriving (Show)

instance Eq LRCollection where
    (==) (LRCollection _ items1) (LRCollection _ items2) = items1 == items2

instance Ord LRCollection where
    compare (LRCollection _ items1) (LRCollection _ items2) = compare items1 items2

data DFA = DFA { collections :: Set.Set LRCollection
               , dfa_symbols :: Set.Set Symbol
               , start_collection :: LRCollection
               , goto :: LRCollection -> Symbol -> Maybe LRCollection
               }

instance Show DFA where
    show dfa = collections_str ++ "\n" ++ start_collection_str ++ "\n" ++ goto_str
        where
            collections' = List.sortOn (\(LRCollection index _) -> index) $ Set.toList $ collections dfa
            collections_str = "Collections:" ++ (List.foldl (\str collection -> str ++ "\n" ++ (show collection)) "" collections')
            start_collection_str = "Start collection:\n" ++ (show $ start_collection dfa)
            goto_str = "Goto:" ++ (display_goto_str dfa collections')
                where
                    display_goto_str _ [] = ""
                    display_goto_str dfa (collection:remain) = display_goto_str' dfa collection (Set.toList $ dfa_symbols dfa) ++ (display_goto_str dfa remain)
                        where
                            display_goto_str' _ _ [] = ""
                            display_goto_str' dfa collection@(LRCollection index1 _) (symbol:remain) = str ++ display_goto_str' dfa collection remain
                                where
                                    next = goto dfa collection symbol
                                    str = case next of
                                            Just (LRCollection index2 _) -> "\n" ++ (show index1) ++ " -" ++ (show symbol) ++ "-> " ++ (show index2)
                                            Nothing -> ""

grammar_to_DFA :: Grammar -> DFA
grammar_to_DFA grammar = dfa
    where
        grammar' = augment_grammar grammar
        start_symbol' = start_symbol grammar'
        ini_start_symbol = Set.elemAt 0 $ productions grammar' start_symbol'
        start_item = init_item start_symbol' ini_start_symbol (Set.singleton EOF)
        start_collection' = LRCollection 0 $ closure_items grammar' $ Set.singleton start_item
        dfa' = DFA { collections = Set.singleton start_collection'
                   , dfa_symbols = symbols grammar'
                   , start_collection = start_collection'
                   , goto = (\_ _ -> Nothing)
                   }
        dfa = grammar_to_DFA' grammar' [start_collection'] dfa'

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
                        else goto dfa collection' symbol)
        dfa' = if Set.null new_items
                then dfa
                else dfa { collections = collections'
                         , goto = goto'
                         }
