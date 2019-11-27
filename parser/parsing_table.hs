module Parsing_Table where

import CFG
import DFA

import qualified Data.Set as Set
import qualified Data.List as List

data Action = Shift LRCollection | Reduce Symbol | Accept | Reject

member_of :: LRItem -> LRCollection -> Bool
member_of item (LRCollection _ items) = Set.member item items

action :: Grammar -> DFA -> LRCollection -> Symbol -> Action
action grammar dfa collection@(LRCollection _ items) symbol = action'
    where
        shift_collection = goto dfa collection symbol
        reducible_items = Set.filter (\item -> is_reducible item) items
        start_symbol' = start_symbol grammar
        start_item = LRItem start_symbol' (Set.elemAt 0 $ productions grammar start_symbol') 1
        start_items = Set.filter (\item -> item == start_item) items
        action' = if not $ Set.null start_items
                    then Accept
                    else
                        case shift_collection of
                            Just next_collection -> Shift next_collection
                            Nothing ->
                                if not $ Set.null reducible_items
                                    then
                                    else Reject