import Core
import Input_Parser
import String_Matcher

import System.IO
import System.Environment

main = do
    args <- getArgs
    if length args /= 3
        then error "There must be THREE parameters."
        else do
            let edges_path = args !! 0
                end_states_path = args !! 1
                string_path = args !! 2
            edges_str <- readFile edges_path
            end_states_str <- readFile end_states_path
            string_str <- readFile string_path
            let fa = update_ids (edges_to_fa edges_str) end_states_str
                tokens = match_string string_str fa []
                just_tokens = case tokens of
                                Just tokens -> tokens
                                Nothing -> error "Match error"
            mapM_ print just_tokens