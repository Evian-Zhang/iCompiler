import Re
import NFA
import DFA
import DFAO

main = do
    putStrLn "Input the regular expression: "
    regular_expression <- getLine
    let postfix_regular_expression = shunting_yard $ tokenize_regular_expression regular_expression
        nfa = regular_tokens_to_NFA postfix_regular_expression
        dfa = nfa_to_dfa nfa
        dfao = dfa_to_dfao dfa
    putStrLn "---------NFA---------"

    putStrLn "---------DFA---------"
    putStrLn "----minimized DFA----"
