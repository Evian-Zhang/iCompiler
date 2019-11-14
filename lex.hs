import Re
import NFA
import DFA
import DFAO

import System.IO
import System.Environment

input priority handle expressions = do
    id <- hGetLine handle
    if null id
        then return (expressions)
        else do
            regular_expression <- hGetLine handle
            re_id <- REID priority id
            input (priority + 1) handle ((re_id, regular_expression):expressions)

    

main = do
    args <- getArgs
    file_path <- args !! 1
    handle <- openFile file_path ReadMode
    expressions <- input 0 handle []
    let postfix_regular_expressions = map (\(id, regular_expression) -> (id, shunting_yard $ tokenize_regular_expression regular_expression)) expressions
        nfa = regular_expressions_to_NFA postfix_regular_expressions
        dfa = nfa_to_dfa nfa
        dfao = dfa_to_dfao dfa
    putStrLn "---------NFA---------"
    print nfa
    putStrLn "---------DFA---------"
    print dfa
    putStrLn "----minimized DFA----"
    print dfao
