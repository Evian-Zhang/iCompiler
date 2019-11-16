module InputParser
( edges_to_fa
, update_ids
) where

import Core

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

update_charset :: FA -> [Char] -> FA
update_charset fa cs = FA { states = states fa
                          , charset = cs
                          , edges = edges fa
                          , start_state = start_state fa
                          , end_states = end_states fa
                          , ids = ids fa
                          }

update_state :: FA -> [String] -> FA
update_state fa (header:body) = FA { states = states fa ++ [the_state]
                                   , charset = charset fa
                                   , edges = edges'
                                   , start_state = start_state fa
                                   , end_states = end_states fa
                                   , ids = ids fa
                                   }
    where
        the_index = read header
        the_state = State the_index
        charset' = charset fa
        maps = zip [0, 1..] body
        edges' = foldl (\edge (index, result) ->
            if length result > 0
                then (\state@(State index') c -> 
                    if index' == the_index && c == charset' !! index
                        then Just $ State $ read result
                        else edge state c)
                else edge) (edges fa) maps

update_ids' :: FA -> [String] -> FA
update_ids' fa (header:body:_) = FA { states = states fa
                                    , charset = charset fa
                                    , edges = edges fa
                                    , start_state = start_state fa
                                    , end_states = end_states fa ++ [the_state]
                                    , ids = ids'
                                    }
    where
        the_state = State $ read header
        ids' = (\state -> if state == the_state then Just $ ID $ read body else ids fa the_state)

edges_to_fa :: String -> FA
edges_to_fa str = fa
    where
        fa1 = empty_fa
        (cs:body) = lines str
        fa2 = update_charset fa1 $ tail cs
        fa = foldl (\fa' line -> update_state fa' $ wordsWhen (==',') line) fa2 body

update_ids :: FA -> String -> FA
update_ids fa str = fa'
    where
        strs = lines str
        fa' = foldl (\fa'' line -> update_ids' fa'' $ wordsWhen (==',') line) fa strs