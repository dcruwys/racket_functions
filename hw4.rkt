#lang racket
(require parser-tools/lex
         parser-tools/yacc)

(define-tokens n (VALUE BINARYOP UNARYOP ID))
(define-empty-tokens e (LEFTPAREN RIGHTPAREN EOF))
;;HW4.1
;<exp> ::= <id> | <level 1>
;<id> ::= true | false
;<level 1> ::= <level 1> XOR <level 2> | <level 2>
;<level 2> ::= <level 2> OR <level 3> | <level 3>
;<level 3> ::= <level 3> AND <level 4> | <level 4>
;<level 4> ::= NOT <exp> | (exp)

;;HW4.2 and HW4.3
(define constantBoolLexer
  (lexer
    ["TRUE" (token-VALUE lexeme)]
    ["FALSE" (token-VALUE lexeme)]
    [#\( (token-LEFTPAREN)]
    [#\) (token-RIGHTPAREN)]
    ["AND" (token-BINARYOP lexeme)]
    ["XOR" (token-BINARYOP lexeme)]
    ["OR" (token-BINARYOP lexeme)]
    ["NOT" (token-UNARYOP lexeme)]
    [(concatenation alphabetic (repetition 0 +inf.0 (union alphabetic numeric))) (token-ID lexeme)]
    [whitespace (constantBoolLexer input-port)]
    [(eof) (token-EOF)]
    ))

(define (getTokens lex in)
  (let [(token (lex in))]
    (cond [(equal? token 'EOF) '()]
          [else (cons token (getTokens lex in))])))

(define input (open-input-string "TRUE AND FALSE OR TRUE E1ST"))

                          
