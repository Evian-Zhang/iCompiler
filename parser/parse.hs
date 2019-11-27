import CFG
import Input_Parser
import DFA

import System.IO
import System.Environment

main = do
    args <- getArgs
    if length args /= 1
        then error "There must be One parameters."
        else do
            let file_path = args !! 0
            contents <- readFile file_path
            let grammar = build_grammar contents
                dfa = grammar_to_DFA grammar
            print dfa