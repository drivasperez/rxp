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



<svg width="927pt" height="44pt"
 viewBox="0.00 0.00 927.37 44.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph0" class="graph" transform="scale(1 1) rotate(0) translate(4 40)">
<title>Scanner</title>
<polygon fill="white" stroke="transparent" points="-4,4 -4,-40 923.37,-40 923.37,4 -4,4"/>
<!-- 0 -->
<g id="node1" class="node">
<title>0</title>
<ellipse fill="none" stroke="black" cx="55.9" cy="-18" rx="55.79" ry="18"/>
<text text-anchor="middle" x="55.9" y="-14.3" font-family="Times,serif" font-size="14.00">LeftParen</text>
</g>
<!-- 1 -->
<g id="node2" class="node">
<title>1</title>
<ellipse fill="none" stroke="black" cx="174.79" cy="-18" rx="27" ry="18"/>
<text text-anchor="middle" x="174.79" y="-14.3" font-family="Times,serif" font-size="14.00">a</text>
</g>
<!-- 0&#45;&gt;1 -->
<g id="edge1" class="edge">
<title>0&#45;&gt;1</title>
<path fill="none" stroke="black" d="M111.92,-18C120.57,-18 129.31,-18 137.4,-18"/>
<polygon fill="black" stroke="black" points="137.59,-21.5 147.59,-18 137.59,-14.5 137.59,-21.5"/>
</g>
<!-- 2 -->
<g id="node3" class="node">
<title>2</title>
<ellipse fill="none" stroke="black" cx="268.99" cy="-18" rx="31.4" ry="18"/>
<text text-anchor="middle" x="268.99" y="-14.3" font-family="Times,serif" font-size="14.00">Pipe</text>
</g>
<!-- 1&#45;&gt;2 -->
<g id="edge2" class="edge">
<title>1&#45;&gt;2</title>
<path fill="none" stroke="black" d="M201.96,-18C209.84,-18 218.67,-18 227.25,-18"/>
<polygon fill="black" stroke="black" points="227.51,-21.5 237.51,-18 227.51,-14.5 227.51,-21.5"/>
</g>
<!-- 3 -->
<g id="node4" class="node">
<title>3</title>
<ellipse fill="none" stroke="black" cx="363.18" cy="-18" rx="27" ry="18"/>
<text text-anchor="middle" x="363.18" y="-14.3" font-family="Times,serif" font-size="14.00">b</text>
</g>
<!-- 2&#45;&gt;3 -->
<g id="edge3" class="edge">
<title>2&#45;&gt;3</title>
<path fill="none" stroke="black" d="M300.43,-18C308.64,-18 317.59,-18 326.07,-18"/>
<polygon fill="black" stroke="black" points="326.1,-21.5 336.1,-18 326.1,-14.5 326.1,-21.5"/>
</g>
<!-- 4 -->
<g id="node5" class="node">
<title>4</title>
<ellipse fill="none" stroke="black" cx="489.23" cy="-18" rx="63.09" ry="18"/>
<text text-anchor="middle" x="489.23" y="-14.3" font-family="Times,serif" font-size="14.00">RightParen</text>
</g>
<!-- 3&#45;&gt;4 -->
<g id="edge4" class="edge">
<title>3&#45;&gt;4</title>
<path fill="none" stroke="black" d="M390.19,-18C397.91,-18 406.77,-18 415.94,-18"/>
<polygon fill="black" stroke="black" points="415.97,-21.5 425.97,-18 415.97,-14.5 415.97,-21.5"/>
</g>
<!-- 5 -->
<g id="node6" class="node">
<title>5</title>
<ellipse fill="none" stroke="black" cx="618.82" cy="-18" rx="30.59" ry="18"/>
<text text-anchor="middle" x="618.82" y="-14.3" font-family="Times,serif" font-size="14.00">Star</text>
</g>
<!-- 4&#45;&gt;5 -->
<g id="edge5" class="edge">
<title>4&#45;&gt;5</title>
<path fill="none" stroke="black" d="M552.53,-18C561.15,-18 569.81,-18 577.87,-18"/>
<polygon fill="black" stroke="black" points="578.08,-21.5 588.08,-18 578.08,-14.5 578.08,-21.5"/>
</g>
<!-- 6 -->
<g id="node7" class="node">
<title>6</title>
<ellipse fill="none" stroke="black" cx="712.37" cy="-18" rx="27" ry="18"/>
<text text-anchor="middle" x="712.37" y="-14.3" font-family="Times,serif" font-size="14.00">c</text>
</g>
<!-- 5&#45;&gt;6 -->
<g id="edge6" class="edge">
<title>5&#45;&gt;6</title>
<path fill="none" stroke="black" d="M649.54,-18C657.71,-18 666.66,-18 675.14,-18"/>
<polygon fill="black" stroke="black" points="675.19,-21.5 685.19,-18 675.19,-14.5 675.19,-21.5"/>
</g>
<!-- 7 -->
<g id="node8" class="node">
<title>7</title>
<ellipse fill="none" stroke="black" cx="802.37" cy="-18" rx="27" ry="18"/>
<text text-anchor="middle" x="802.37" y="-14.3" font-family="Times,serif" font-size="14.00">d</text>
</g>
<!-- 6&#45;&gt;7 -->
<g id="edge7" class="edge">
<title>6&#45;&gt;7</title>
<path fill="none" stroke="black" d="M739.77,-18C747.76,-18 756.68,-18 765.19,-18"/>
<polygon fill="black" stroke="black" points="765.29,-21.5 775.29,-18 765.29,-14.5 765.29,-21.5"/>
</g>
<!-- 8 -->
<g id="node9" class="node">
<title>8</title>
<ellipse fill="none" stroke="black" cx="892.37" cy="-18" rx="27" ry="18"/>
<text text-anchor="middle" x="892.37" y="-14.3" font-family="Times,serif" font-size="14.00">e</text>
</g>
<!-- 7&#45;&gt;8 -->
<g id="edge8" class="edge">
<title>7&#45;&gt;8</title>
<path fill="none" stroke="black" d="M829.77,-18C837.76,-18 846.68,-18 855.19,-18"/>
<polygon fill="black" stroke="black" points="855.29,-21.5 865.29,-18 855.29,-14.5 855.29,-21.5"/>
</g>
</g>
</svg>
