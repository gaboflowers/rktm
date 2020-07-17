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
         parse-declare
         parse-comma-values-semicol
         get-semicol-line
         get-for-loop
         get-block
         get-balanced
         ctm-parser-test)

(define (error-syntax [msg #f])
  (if msg
    (error 'Syntax-error msg)
    (error 'Syntax-error)))

(define (error-index [msg #f])
  (if msg
    (error 'Index-error msg)
    (error 'Index-error)))


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

; unwrap-for: ForLoop List[Cons[<id> <id>]] (List[tokens] List[Cons[<id> <id>]] -> List[tokens]) -> List[tokens]
(define (unwrap-for for-loop index-subst callback)
  (match-define (ForLoop (list 'ID id) (list 'INT low) (list 'INT high) body) for-loop)
  (when (not (integer? low))
    (error-syntax (format "Lower bound ~a is not an integer" low)))
  (when (not (integer? high))
    (error-syntax (format "Upper bound ~a is not an integer" high)))
  (when (> low high)
    (error-index (format "Index ~a goes from ~a up to ~a~" id low high)))
  (let loop ([current low] [acc '()])
    (if (> current high)
      (reverse acc)
      (loop (add1 current)
            (cons (car (callback body (cons (cons id current)
                                     index-subst)))
                  acc)))))

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
   [(list x 'SEMICOL) (cons x '())]
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

(define (parse-declare-statements token-list [index-subst '()] [acc '()])
  (match token-list
    [(cons stmt-head xs)
     (match (declare-statement-type stmt-head)
       ['for-loop
          (match-define (tokens-tuple for-loop rest) (get-for-loop token-list))
          (parse-declare-statements rest index-subst (append (reverse (unwrap-for for-loop index-subst (lambda (body idx-subst)
                                                                                             (parse-declare-statements body idx-subst))))
                                                      acc))]
       ['state-declaration
          (match-define (tokens-tuple line rest) (get-semicol-line token-list))
          (parse-declare-statements rest index-subst
                                         (cons (parse-state-declaration line index-subst)
                                               acc))])]
    ['() (reverse acc)]))


(struct State (id init final) #:transparent)
(define (parse-state-declaration tokens index-subst [init #f] [final #f])
  (match tokens
    [(list 'INIT xs ...) (parse-state-declaration xs index-subst #t final)]
    [(list 'FINAL xs ...) (parse-state-declaration xs index-subst init #t)]
    [(list 'STATE xs ...) (parse-state-declaration xs index-subst init final)]
    [(list (list 'ID id)) (State (list 'ID (evaluate-index id index-subst)) init final)]))

(define (evaluate-index id index-subst)
  (begin
  (define (evaluate-string-index body-id index-subst)
      (match index-subst
        ['() (string->symbol body-id)]
        [(cons (cons index value) xs)
         (let [(index-needle (symbol->string index))]
           (if (string-contains? body-id index-needle)
             (evaluate-string-index (string-replace body-id index-needle (number->string value)) xs)
             (error-index (format "State identifier ~a does not contain ~a as a substring" body-id index-needle))))])))
  (evaluate-string-index (symbol->string id) index-subst))

;; Parse transition
(define (parse-transition tokens)
  #f)

(define (lex-and-parse prog)
  (ctm-parse (ctm-lexer prog)))


; Test
(require test-engine/racket-tests)

(check-expect (tokens-tuple-main (get-balanced '{ 1 2 lp 3 4 lp 5 6 rp 7 lp 8 9 rp 10 rp 11 rp 12 } 'lp 'rp))
              '(3 4 lp 5 6 rp 7 lp 8 9 rp 10))
(check-expect (evaluate-index 'qPi (list (cons 'P 4) (cons 'i 2)))
              'q42)

(define ctm-parser-test (lambda () (test)))
