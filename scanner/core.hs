module Core 
( State (State)
, ID (ID)
, FA (FA)
, states
, charset
, edges
, start_state
, end_states
, ids
, empty_fa
) where

data State = State Int deriving (Eq, Ord, Show)

data ID = ID String deriving (Show)

data FA = FA { states :: [State]
             , charset :: [Char]
             , edges :: State -> Char -> Maybe State
             , start_state :: State
             , end_states :: [State]
             , ids :: State -> Maybe ID
             }

empty_fa :: FA
empty_fa = FA { states = []
              , charset = []
              , edges = (\_ _ -> Nothing)
              , start_state = State 0
              , end_states = []
              , ids = (\_ -> Nothing)
              }