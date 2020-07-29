#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(provide ctm-lexer)

; http://matt.might.net/articles/lexers-in-racket/

(define ctm-lexer
  (lexer
    [(:: #\/ #\*) (comment-block-lexer input-port)]
    [(:: #\/ #\/) (comment-line-lexer input-port)]

    [#\{ (cons 'LBRA (ctm-lexer input-port))]
    [#\} (cons 'RBRA (ctm-lexer input-port))]
    [#\( (cons 'LPAR (ctm-lexer input-port))]
    [#\) (cons 'RPAR (ctm-lexer input-port))]
    [#\; (cons 'SEMICOL (ctm-lexer input-port))]
    [#\, (cons 'COMMA (ctm-lexer input-port))]
    [#\= (cons 'EQ (ctm-lexer input-port))]

    [(:: (:? #\-) (:+ (char-range #\0 #\9)))
        (cons `(INT ,(string->number lexeme)) (ctm-lexer input-port))]

    ["state" (cons 'STATE (ctm-lexer input-port))]
    ["for" (cons 'FOR (ctm-lexer input-port))]
    ["from" (cons 'FROM (ctm-lexer input-port))]
    ["to" (cons 'TO (ctm-lexer input-port))]

    ["setup" (cons 'SETUP (ctm-lexer input-port))]
    ["alias" (cons 'ALIAS (ctm-lexer input-port))]
    ["declare" (cons 'DECLARE (ctm-lexer input-port))]
    ["transition" (cons 'TRANSITION (ctm-lexer input-port))]

    ; setup
    ["alphabet" (cons 'STP-ALPHABET (ctm-lexer input-port))]
    ["blank_symbol" (cons 'STP-BLANK_SYMBOL (ctm-lexer input-port))]
    ["init_tape_size" (cons 'STP-INIT_TAPE_SIZE (ctm-lexer input-port))]
    ["max_tape_size" (cons 'STP-MAX_TAPE_SIZE (ctm-lexer input-port))]
    ["max_number_transitions" (cons 'STP-MAX_NUM_TRANSITIONS (ctm-lexer input-port))]
    ["print_status" (cons 'STP-PRINT_STATUS (ctm-lexer input-port))]
    ["allow_partial_accept" (cons 'STP-ALLOW_PARTIAL_ACCEPT (ctm-lexer input-port))]

    ; declare
    ["init" (cons 'INIT (ctm-lexer input-port))]
    ["final" (cons 'FINAL (ctm-lexer input-port))]

    ; transition
    ["when" (cons 'WHEN (ctm-lexer input-port))]
    ["if" (cons 'IF (ctm-lexer input-port))]
    ["else" (cons 'ELSE (ctm-lexer input-port))]
    ["forall" (cons 'FORALL (ctm-lexer input-port))]
    ["read" (cons 'READ (ctm-lexer input-port))]
    ["goto" (cons 'GOTO (ctm-lexer input-port))]
    ["write" (cons 'WRITE (ctm-lexer input-port))]
    ["header" (cons 'HEADER (ctm-lexer input-port))]
    ["blank" (cons 'BLANK (ctm-lexer input-port))]

    [#\> (cons 'RHEAD (ctm-lexer input-port))]
    [#\< (cons 'LHEAD (ctm-lexer input-port))]

    [#\+ (cons 'INC (ctm-lexer input-port))]
    [#\- (cons 'DEC (ctm-lexer input-port))]

    [(:: (:or (char-range #\a #\z) (char-range #\A #\Z) #\_) (:* (:or (char-range #\a #\z) (char-range #\A #\Z) #\_ (char-range #\0 #\9)))) ; [A-Za-z_]\w*
        (cons `(ID ,(string->symbol lexeme)) (ctm-lexer input-port))]

    [whitespace (ctm-lexer input-port)]
    [(eof) '()]
    ))

(define comment-block-lexer
  (lexer
   [(:: #\* #\/) (ctm-lexer input-port)]
   [any-char (comment-block-lexer input-port)]))

(define comment-line-lexer
  (lexer
   [#\newline (ctm-lexer input-port)]
   [any-char (comment-line-lexer input-port)]))
