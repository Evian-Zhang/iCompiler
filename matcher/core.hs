module Core
( Symbol (Normal, Epsilon, EOF)
, Production (Production)
, Action (Shift, Reduce, Accept, Reject)
, DFA (DFA)
, ParseTree (ParseTree)
, collections
, action
, goto
, singleton_dfa
)
where

import qualified Data.Set as Set
import qualified Data.List as List

data Symbol = Normal String | Epsilon | EOF deriving (Eq)

instance Show Symbol where
    show symbol = case symbol of
        Normal string -> string
        Epsilon -> "ε"
        EOF -> "$"

data Production = Production Symbol [Symbol]

instance Show Production where
    show (Production lhs rhs) = show lhs ++ "->" ++ (List.foldl (\str symbol -> str ++ (show symbol)) "" rhs)

data Action = Shift Int | Reduce Production | Accept | Reject deriving (Show)

data DFA = DFA { collections :: Set.Set Int
               , action :: Int -> Symbol -> Action
               , goto :: Int -> Symbol -> Maybe Int
               }

singleton_dfa :: DFA
singleton_dfa = DFA { collections = Set.empty
                    , action = (\_ _ -> Reject)
                    , goto = (\_ _ -> Nothing)
                    }

data ParseTree = ParseTree Symbol [ParseTree]

instance Show ParseTree where
    show (ParseTree lhs rhs) = show lhs ++ (List.concatMap (\node -> show' node 0) rhs)
        where
            show' (ParseTree lhs rhs) indent = "\n" ++ (List.concat $ List.replicate indent "   ") ++ "|--" ++ (show lhs) ++ (List.concatMap (\node -> show' node (indent + 1)) rhs)