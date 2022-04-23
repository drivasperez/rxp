screenshots:
  cargo run -- dot tokens '(a|b)*cde' | dot -Tpng > images/tokens_example.png && \
  cargo run -- dot ast '(a|b)*cde' | dot -Tpng > images/ast_example.png && \
  cargo run -- dot nfa '(a|b)*cde' | dot -Tpng > images/nfa_example.png && \
  cargo run -- dot dfa '(a|b)*cde' | dot -Tpng > images/dfa_example.png && \
  cargo run -- dot vm '(a|b)*cde' | dot -Tpng > images/vm_example.png && \
  cargo run -- dot vmtree '(a|b)*cde' | dot -Tpng > images/vmtree_example.png
