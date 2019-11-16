module String_Matcher
( match_string ) where

import Core

data Token = Token String ID deriving (Show)

match_string :: String -> FA -> [Token] -> Maybe [Token]
match_string string fa tokens = case maybe_token of
        Just token -> match_string (drop (length matched_string) string) fa (tokens ++ [token])
        Nothing -> if length string == 0
                    then Just tokens
                    else Nothing
    where
        (matched_string, maybe_token) = match_one_token string fa (start_state fa) ("", Nothing)


match_one_token :: String -> FA -> State -> (String, Maybe Token) -> (String, Maybe Token)
match_one_token string fa current_state (matched_string, matched_token) = (matched_string', matched_token')
    where
        (matched_string', matched_token') = case edges fa current_state $ head string of
            Just next_state -> case ids fa current_state of
                Just id -> match_one_token (tail string) fa next_state new_pair
                    where
                        new_string = matched_string ++ [head string]
                        new_pair = (new_string, Just $ Token new_string id)
                Nothing -> match_one_token (tail string) fa next_state (matched_string ++ [head string], matched_token)
            Nothing -> (matched_string, matched_token)