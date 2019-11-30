import CFG
import Input_Parser
import DFA
import Parsing_Table
import Output

import System.IO
import System.Environment

import qualified Data.Set as Set

main = do
    args <- getArgs
    if length args /= 3
        then error "There must be THREE parameters."
        else do
            let file_path = args !! 0
                action_path = args !! 1
                goto_path = args !! 2
            contents <- readFile file_path
            let grammar = build_grammar contents
                dfa = grammar_to_DFA grammar
                action_str = output_action grammar dfa
                goto_str = output_goto grammar dfa
            print dfa
            writeFile (args !! 1) action_str
            writeFile (args !! 2) goto_str