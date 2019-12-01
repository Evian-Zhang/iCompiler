import Core
import Input_Parser
import Stacker

import System.IO
import System.Environment

main = do
    args <- getArgs
    if length args /= 3
        then error "There must be THREE parameters."
        else do
            let action_path = args !! 0
                goto_path = args !! 1
                string_path = args !! 2
            action_string <- readFile action_path
            goto_string <- readFile goto_path
            string <- readFile string_path
            let dfa = parse_action_goto action_string goto_string
                parsetree = construct_parsetree dfa string
            print parsetree