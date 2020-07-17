#lang racket

(require "lexer.rkt")

(provide ctm-parse
         lex-and-parse
         error-syntax
         tokens-tuple  ; remove
         tokens-tuple-main
         tokens-tuple-rest
         parse-setup
         parse-alias
         get-semicol-line
         get-for-loop
         get-block
         get-balanced
         ctm-parser-test)

(define (error-syntax [msg #f])
  (if msg
    (error 'Syntax-error msg)
    (error 'Syntax-error)))

(struct tokens-tuple (main rest))

; forward: List x literal -> List
; Gets the `ls` elements only after the first occurence of `needle`
; (equivalent to (cdr (member needle ls)))
(define (forward ls needle)
  (match ls
       ['() ls]
       [(list (? (lambda (c) (equal? needle c)) x) xs ...) xs]
       [_ (forward (cdr ls) needle)]))

; get-balanced: List[tokens] x literal x literal -> tokens-tuple/error "Syntax"
; Returns the contents of the list delimited by `left` and `right`
; (not inclusively) with proper balance.
(define (get-balanced ls left right)
  (if (equal? ls '())
      '()
      (let loop ([bal 1] [l (forward ls left)] [acc '()])
        (if (equal? l '())
          (error-syntax (format "Unbalanced statement in ~a" ls))
          (match l
            [(cons (? (lambda (x) (equal? x left))) xs) (loop (add1 bal) xs (cons left acc))]
            [(cons #;(value-pat right) ; no funciona como macro 🙄, hay que usar https://stackoverflow.com/a/35593086
                   (? (lambda (x) (equal? x right))) xs)
                        (if (> bal 1)
                            (loop (sub1 bal) xs (cons right acc))
                            (tokens-tuple (reverse acc) xs))] ; <- return
            [(cons x xs) (loop bal xs (cons x acc))])))))


; get-block: List[tokens] x <any> [x literal x literal] -> List[tokens]
; Returns the contents of the block delimited by 'LBRA ... 'RBRA,
; with proper bracket balance. If `previous` is passed, will start from
; the first occurence of the token
(define (get-block tokens [previous #f] [lbra 'LBRA] [rbra 'RBRA])
  (let ([tokens (member lbra (if previous
                              (forward tokens previous)
                              tokens))])
    (if (not tokens)
      '()
      (let loop ([block (get-balanced tokens lbra rbra)])
        #f
        #|
        (match (car l)
               ...
        )|#
      ))))

;; Semicol Lines
(define (get-semicol-line tokens [acc '()])
  (match tokens
    [(list 'SEMICOL xs ...) (tokens-tuple (reverse acc) xs)]
    [(list x xs ...) (get-semicol-line xs (cons x acc))]))

;; For loops
(struct ForLoop (id low high body) #:transparent)

; get-for-loop: List[tokens] -> tokens-tuple[ForLoop List[tokens]]
(define (get-for-loop tokens)
  (match tokens
    [(list 'FOR xs ...)
     (match-define (tokens-tuple for-args args-rest) (get-balanced xs 'LPAR 'RPAR))
     (match-define (tokens-tuple for-body rest) (get-balanced args-rest 'LBRA 'RBRA))
     (match-define (list id 'FROM low 'TO high) for-args)
     (tokens-tuple (ForLoop id low high for-body) rest)]))

(define (unwrap-for for-loop)
  ; TO DO
  for-loop)


;; Main parser
(define (ctm-parse tokens)
  (let ([setup-block (get-block tokens 'SETUP)]
        [alias-block (get-block tokens 'ALIAS)]
        [declare-block (get-block tokens 'DECLARE)]
        [transition-block (get-block tokens 'TRANSITION)])
    #|
    (when (member '() (list setup-block declare-block transition-block))
        (error-syntax "Does not conform to `setup {} [ alias {} ] declare {} transition {}"))
    |#
    (list (parse-setup setup-block)
          (parse-alias alias-block)
          (parse-declare declare-block)
          (parse-transition transition-block))))

;; Parse setup
(define (parse-setup tokens)
  (match tokens
    [(list 'SETUP 'LBRA setup-token-list ... 'RBRA) (parse-setup-statements setup-token-list)]))

(define (setup-statement-type token)
  (case token
    [(STP-ALPHABET) 'multiple]
    [(STP-BLANK_SYMBOL STP-MAX_TAPE_SIZE STP-MAX_NUM_TRANSITIONS) 'single]))

(define (parse-setup-statements token-list [acc '()])
  (match token-list
    [(cons stmt-name xs)
     (match (setup-statement-type stmt-name)
       ['multiple
          (match-define (tokens-tuple body rest) (get-balanced xs 'LBRA 'RBRA))
          (parse-setup-statements rest (cons (parse-setup-multiple-statement stmt-name body)
                                             acc))]
       ['single
          (match xs
             [(list 'EQ rhs 'SEMICOL xs ...)  (parse-setup-statements xs (cons (parse-setup-single-statement stmt-name rhs)
                                                                      acc))])])]
    ['() (reverse acc)]))

(struct Alphabet (token-list) #:transparent)

(define (parse-setup-multiple-statement stmt-name body)
  (match stmt-name
   ['STP-ALPHABET (Alphabet (parse-comma-values-semicol body))]))

(define (parse-setup-single-statement stmt-name rhs)
  (cons stmt-name rhs)) ; pa qué más

(define (parse-comma-values-semicol token-list)
  (match token-list
   [(list x 'SEMICOL) x]
   [(list x 'COMMA xs ...) (cons x (parse-comma-values-semicol xs))]))

;; Parse alias
(define (parse-alias tokens)
  (match tokens
    [(list 'ALIAS 'LBRA alias-token-list ... 'RBRA) (parse-alias-statements alias-token-list)]))

(define (parse-alias-statements token-list [acc '()])
  (match token-list
    [(list 'STATE lhs 'EQ rhs 'SEMICOL xs ...) (parse-alias-statements xs
                                                                      (cons (cons lhs rhs) acc))]
    ['() (reverse acc)]))

;; Parse declare
(define (parse-declare tokens)
  (match tokens
    [(list 'DECLARE 'LBRA declare-token-list ... 'RBRA) (parse-declare-statements declare-token-list)]))

(define (declare-statement-type token)
  (case token
    [(INIT FINAL STATE) 'state-declaration]
    [(FOR) 'for-loop]))

(define (parse-declare-statements token-list [acc '()])
  (match token-list
    [(cons stmt-head xs)
     (match (declare-statement-type stmt-head)
       ['for-loop
          (match-define (tokens-tuple for-loop rest) (get-for-loop token-list))
          (parse-declare-statements rest (cons (unwrap-for for-loop)
                                                acc))]
       ['state-declaration  #| TODO |# #f])]
    ['() (reverse acc)]))


;; Parse transition
(define (parse-transition tokens)
  #f)

(define (lex-and-parse prog)
  (ctm-parse (ctm-lexer prog)))


; Test
(require test-engine/racket-tests)

(check-expect (tokens-tuple-main (get-balanced '{ 1 2 lp 3 4 lp 5 6 rp 7 lp 8 9 rp 10 rp 11 rp 12 } 'lp 'rp))
              '(3 4 lp 5 6 rp 7 lp 8 9 rp 10))

(define ctm-parser-test (lambda () (test)))
