module String_Matcher
( match_string
, MatchResult (Success, Fail)
) where

import Core

data Token = Token String ID

instance Show Token where
    show (Token str id) = "Token \"" ++ str ++ "\" " ++ (show id)

data MatchResult = Success [Token] | Fail String

match_string :: String -> FA -> [Token] -> MatchResult
match_string string fa tokens = case maybe_token of
        Just token@(Token matched_string _) -> match_string (drop (length matched_string) string) fa (tokens ++ [token])
        Nothing -> if length string == 0
                    then Success tokens
                    else Fail string
    where
        (_, maybe_token) = match_one_token string fa (start_state fa) ("", Nothing)


match_one_token :: String -> FA -> State -> (String, Maybe Token) -> (String, Maybe Token)
match_one_token string fa current_state (matched_string, matched_token) = (matched_string', matched_token')
    where
        (matched_string', matched_token') = if length string > 0
            then
                case edges fa current_state $ head string of
                    Just next_state -> case ids fa next_state of
                        Just id -> match_one_token (tail string) fa next_state new_pair
                            where
                                new_string = matched_string ++ [head string]
                                new_pair = (new_string, Just $ Token new_string id)
                        Nothing -> match_one_token (tail string) fa next_state (matched_string ++ [head string], matched_token)
                    Nothing -> (matched_string, matched_token)
            else (matched_string, matched_token)