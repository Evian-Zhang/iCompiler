module Parsing_Table
( Action (Shift, Reduce, Accept, Reject)
, Production
, action
) where

import CFG
import DFA

import qualified Data.Set as Set
import qualified Data.List as List

data Production = Production Symbol RHS

instance Show Production where
    show (Production lhs rhs) = (show lhs) ++ "->" ++ (List.foldl (\rhs_str symbol -> rhs_str ++ (show symbol)) "" rhs)

item_to_production :: LRItem -> Production
item_to_production item = Production (item_lhs item) (item_rhs item)

data Action = Shift LRCollection | Reduce Production | Accept | Reject

action :: Grammar -> DFA -> LRCollection -> Symbol -> Action
action grammar dfa collection@(LRCollection _ items) symbol = action'
    where
        shift_collection = goto dfa collection symbol
        reducible_items = Set.filter (\item -> 
            (is_reducible item) && (Set.member symbol $ item_lookaheads item)) items
        start_symbol' = start_symbol grammar
        start_item = LRItem { item_lhs = start_symbol'
                            , item_rhs = Set.elemAt 0 $ productions grammar start_symbol'
                            , item_dot = 1
                            , item_lookaheads = Set.singleton EOF
                            }
        action' = if Set.member start_item items && symbol == EOF
                    then
                        case shift_collection of
                            Just _ -> error "Shift-reduce conflict"
                            Nothing -> Accept
                    else
                        case shift_collection of
                            Just next_collection -> 
                                if not $ Set.null reducible_items
                                    then error "Shift-reduce conflict"
                                    else Shift next_collection
                            Nothing ->
                                if not $ Set.null reducible_items
                                    then 
                                        if Set.size reducible_items > 1
                                            then error "Reduce-reduce conflict"
                                            else Reduce $ item_to_production $ Set.elemAt 0 reducible_items
                                    else Reject