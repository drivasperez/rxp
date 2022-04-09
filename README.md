# rex

Rex is a command-line utility for visualising and testing simple regular expressions. It takes regular expressions as text input. It has two subcommands, `dot` and `test`

## `rex dot <phase> <regex>`

`rex dot` takes a simple regular expression as text input, and outputs various abstract representations of the regex at different phases of compilation. The output format is the GraphViz `dot` language, which can be visualised with the `dot(1)` utility.

### Output formats: 

- `rex dot tokens <regex>`: Outputs the regular expression as a stream of tokens.
- `rex dot ast <regex>`: Outputs the regular expression as an abstract syntax tree.
- `rex dot nfa <regex>`: Outputs the regular expression as an NFA (Non-deterministic finite automaton).
- `rex dot dfa <regex>`: Outputs the regular expression as a DFA (Deterministic finite automaton).

### Examples:

- `rex dot tokens '(a|b)*cde'`: 
  ![Example of the rex dot tokens command](images/tokens_example.png)

- `rex dot ast '(a|b)*cde'`: 
  ![Example of the rex dot ast command](images/ast_example.png)

- `rex dot nfa '(a|b)*cde'`: 
  ![Example of the rex dot nfa command](images/nfa_example.png)

- `rex dot dfa '(a|b)*cde'`: 
  ![Example of the rex dot dfa command](images/dfa_example.png)
