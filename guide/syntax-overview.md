# 🔤 Syntax Overview

NebuLang's syntax is designed to be simple and minimalistic, making it easy to read and write code. This section provides an overview of NebuLang's syntax using Backus-Naur Form (BNF) notation.

### Backus-Naur Form (BNF)

```rbnf
<program> ::= <inject-statement>* <statement>*

<statement> ::= <set-statement> | <if-statement> | <unless-statement> | <while-statement> | <emit-statement> | <iter-statement> | <exp-def-statement> | <exp-call-statement>
<set-statement> ::= "set" <identifier> ":" <type> "=" <expression> ";"
<if-statement> ::= "if" "(" <expression> ")" <scope> <otherwise>?
<unless-statement> ::= "unless" "(" <expression> ")" <scope> <otherwise>?
<while-statement> ::= "while" "(" <expression> ")" <scope>
<emit-statement> ::= "emit" expression ";"
<iter-statement> ::= "iter" "(" <identifier> ":" <type> "in" <expression> ")" <scope>
<exp-def-statement> ::= "exp" <identifier> "(" <parameter>* ")" <scope>
<exp-call-statement> ::= <exp-call> ";"
<inject-statement> ::= "inject" "(" <identifiers>* ")" "from" <string-literal> ";"

<exp-call> ::= <identifier> "(" <expression>* ")"

<scope> ::= "{" <statement>* "}"
<otherwise> ::= "otherwise" <scope>
<parameter> ::= <identifier> ":" <type>

<type> ::= "Int" | "Char" | "String" | "Bool" | <sequence-type> | "Void"
<sequence-type> ::= "Seq" "[" <type> "]"

<operation> ::= <expression> <operator> <expression>
<operator> ::= <arithmetic-operator> | <logical-operator>
<arithmetic-operator> ::= "+" | "-" | "*" | "/" | "^" | "%"
<logical-operator> ::= "&&" | "||" | "==" | "!=" | "<" | ">" | ">=" | "<="

<comment> ::= "#" <character>

<expression> ::= <literal> | <identifier> | <exp-call> | <operation>

<literal> ::= <int-literal> | <char-literal> | <string-literal> | <bool-literal>
<int-literal> ::= "-"? <digit>+
<char-literal> ::= "'" <character> "'"
<string-literal> ::= "\"" <character>* "\""
<bool-literal> ::= "true" | "false"

<identifier> ::= letter (letter | digit | "_")*

<character> ::= (<letter> | <digit> | <special-char>)*
<letter> ::= "a" | ... | "z" | "A" | ... | "Z"
<digit> ::= "0" | ... | "9"
<special-char> ::= "|" | " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "]" | "^" | "_" | "`" | "{" | "}" | "~"
```
