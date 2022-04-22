screenshots:
  cargo run -- dot tokens '(a|b)*cde' | dot -Tpng > tokens_example.png && \
  cargo run -- dot ast '(a|b)*cde' | dot -Tpng > ast_example.png && \
  cargo run -- dot nfa '(a|b)*cde' | dot -Tpng > nfa_example.png && \
  cargo run -- dot dfa '(a|b)*cde' | dot -Tpng > dfa_example.png && \
  cargo run -- dot vm '(a|b)*cde' | dot -Tpng > vm_example.png && \
  cargo run -- dot vmtree '(a|b)*cde' | dot -Tpng > vmtree_example.png
