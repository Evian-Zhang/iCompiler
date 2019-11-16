import Core
import Input_Parser

import System.IO
import System.Environment

main = do
    args <- getArgs
    if length args /= 3
        then error "There must be THREE parameters."
        else do
            let edges_path = args !! 0
                end_states_path = args !! 1
            edges_str <- readFile edges_path
            end_states_str <- readFile end_states_path
            let fa = update_ids (edges_to_fa edges_str) end_states_str
            putStrLn "Complete"