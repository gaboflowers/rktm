#lang racket

(require "lexer.rkt")

(provide ctm-parser
         lex-and-parse
         error-syntax
         tokens-tuple  ; remove
         tokens-tuple-main
         tokens-tuple-rest
         parse-setup
         parse-alias
         parse-declare
         parse-comma-values-semicol
         parse-transition
         get-semicol-line
         get-for-loop
         get-block
         get-balanced
         group-by-pairs
         ctm-parser-test)

;; https://stackoverflow.com/a/33984471
(define-for-syntax VERBOSE #t)
(define-syntax (if-verbose stx)
  (syntax-case stx ()
    ((_ verb-expr non-verb-expr)
     (if VERBOSE
         #'verb-expr
         #'non-verb-expr))))

(if-verbose
   (define (info-printf . args) (apply printf args))
   (define (info-printf . args) (void)))


(define (error-syntax [msg #f])
  (if msg
    (error 'Syntax-error msg)
    (error 'Syntax-error)))

(define (error-index [msg #f])
  (if msg
    (error 'Index-error msg)
    (error 'Index-error)))

(define (error-setup [msg #f])
  (if msg
    (error 'Setup-error msg)
    (error 'Setup-error)))

(define (error-declare [msg #f])
  (if msg
    (error 'Declaration-error msg)
    (error 'Declaration-error)))

(define (error-state [msg #f])
  (if msg
    (error 'State-error msg)
    (error 'State-error)))


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
; Returns the block preceded by `previous` and delimited by 'LBRA ... 'RBRA,
; with proper bracket balance
(define (get-block tokens previous [lbra 'LBRA] [rbra 'RBRA])
  (let [(found (member previous tokens))]
    (if found
      (letrec [(block-list (tokens-tuple-main (get-balanced found lbra rbra)))
                (append-lst (lambda (lst x)
                              (match lst
                                [(list p) (list p x)]
                                [(list p xs ...) (cons p (append-lst xs x))]
                                ['() '()])))]
        (cons previous
            (cons lbra
                (append-lst block-list rbra))))
        '())))

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
  (info-printf "[unwrap-for] Unwrapping for `~a` from ~v to ~v~n" id low high)
  (when (not (integer? low))
    (error-syntax (format "Lower bound ~a is not an integer" low)))
  (when (not (integer? high))
    (error-syntax (format "Upper bound ~a is not an integer" high)))
  (when (> low high)
    (error-index (format "Index ~a goes from ~a up to ~a~" id low high)))
  (let loop ([current low] [acc '()])
    (if (> current high)
      (reverse acc)
      (begin
      (info-printf "[unwrap-for] > unwrapping `~a` <- ~v~n" id current)
      (loop (add1 current)
            (append (callback body (cons (cons id current)
                                     index-subst))
                  acc)) )  )))

(struct CTM-AST (setup alias declare transition) #:transparent)

;; Main parser
(define (ctm-parser tokens)
  (let ([setup-block (get-block tokens 'SETUP)]
        [alias-block (get-block tokens 'ALIAS)]
        [declare-block (get-block tokens 'DECLARE)]
        [transition-block (get-block tokens 'TRANSITION)])

    (when (member '() (list setup-block declare-block transition-block))
        (error-syntax "Missing block. Does not conform to setup {} [ alias {} ] declare {} transition {}."))

    (let ([ast (CTM-AST (parse-setup setup-block)
                          (parse-alias alias-block)
                          (parse-declare declare-block)
                          (parse-transition transition-block))])
      (check-aliases ast)
      (check-transition-states ast)
      (check-transition-values ast)
      ast)))

;; Parse setup
(define (parse-setup tokens)
  (match tokens
    [(list 'SETUP 'LBRA setup-token-list ... 'RBRA)
     (let ([setup-block (parse-setup-statements setup-token-list)])
       (info-printf "[parse-setup] ~a setup statements parsed~n" (length setup-block))
       setup-block)]))


(define (setup-statement-type token)
  (case token
    [(STP-ALPHABET) 'multiple]
    [(STP-BLANK_SYMBOL STP-MAX_TAPE_SIZE STP-MAX_NUM_TRANSITIONS) 'single]
    [else (error-setup (format "Setup LHS ~a not defined" token))]))

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
    [(list 'ALIAS 'LBRA alias-token-list ... 'RBRA)
     (let ([alias-block (parse-alias-statements alias-token-list)])
       (info-printf "[parse-alias] ~a state aliases parsed~n" (length alias-block))
       alias-block)]))

(define (parse-alias-statements token-list [acc '()])
  (match token-list
    [(list 'STATE lhs 'EQ rhs 'SEMICOL xs ...) (parse-alias-statements xs
                                                                      (cons (cons lhs rhs) acc))]
    ['() (reverse acc)]))

;; Parse declare
(define (parse-declare tokens)
  (match tokens
    [(list 'DECLARE 'LBRA declare-token-list ... 'RBRA)
     (match-let ([(States _ _ all-states) (state-list->states (parse-declare-statements declare-token-list))])
       (info-printf "[parse-declare] ~a states parsed~n" (length all-states)))]))


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
(struct States (init finals all) #:transparent)

(define (state-list->states state-list [init #f] [finals '()] [all '()])
  (match state-list
    ['() (States init (reverse finals) (reverse all))]
    [(cons (State id sinit sfinal) xs)
     (if sinit
         (if init
           (match-let ([(list 'ID init-id) init])
               (error-declare (format "The Machine already has an initial state ~a (found also ~a)" init-id id)))
           (state-list->states xs id (if sfinal (cons id finals)
                                       finals) (cons id all)))
         (if sfinal
           (state-list->states xs init (cons id finals) (cons id all))
           (state-list->states xs init finals (cons id all))))]))

(define (parse-state-declaration tokens index-subst [init #f] [final #f])
  (match tokens
    [(list 'INIT xs ...) (parse-state-declaration xs index-subst #t final)]
    [(list 'FINAL xs ...) (parse-state-declaration xs index-subst init #t)]
    [(list 'STATE xs ...) (parse-state-declaration xs index-subst init final)]
    [(list (list 'ID id)) (State (list 'ID (evaluate-index id index-subst)) init final)]))

(define (evaluate-string-index body-id index-subst)
  (info-printf "[evaluate-string-index] id: ~v, substitutions: ~v~n" body-id index-subst)
  (match index-subst
    ['() (string->symbol body-id)]
    [(cons (cons index value) xs)
     (let [(index-needle (symbol->string index))]
       (if (string-contains? body-id index-needle)
         (evaluate-string-index (string-replace body-id index-needle (number->string value)) xs)
         (error-index (format "State identifier ~a does not contain ~a as a substring" body-id index-needle))))]))

(define (evaluate-index id index-subst)
  (evaluate-string-index (symbol->string id) index-subst))

;; Parse transition
(define (parse-transition tokens)
  (match tokens
    [(list 'TRANSITION 'LBRA transition-token-list ... 'RBRA) (parse-transition-body transition-token-list)]))

(define (parse-transition-body tokens)
  (reverse (parse-transition-statements tokens)))

(define (transition-statement-type token)
  (case token
    [(WHEN) 'when-statement]
    [(FOR) 'for-loop]))

(define (parse-transition-statements token-list [index-subst '()] [acc '()])
  (match token-list
    [(cons stmt-head xs)
     (match (transition-statement-type stmt-head)
       ['for-loop
          (match-define (tokens-tuple for-loop rest) (get-for-loop token-list))
          (parse-transition-statements rest index-subst (append acc (unwrap-for for-loop index-subst (lambda (body idx-subst)
                                                                                             (parse-transition-statements body idx-subst)))
                                                                ))]
       ['when-statement
          (match-define (tokens-tuple when-block rest) (parse-when-block token-list index-subst))
          (parse-transition-statements rest index-subst
                                         (append acc (list when-block)))])]
    ['() (reverse acc)]))

;; When blocks
(struct WhenBlock (state cases) #:transparent)
(struct ConditionalBlock ())
(struct IfBlock (readvalue actions) #:transparent)
(struct IfElseBlock (readvalue actions elseactions) #:transparent)
(struct Forall (actions) #:transparent)

;; Actions
(struct HeaderAction () #:transparent)
(struct IncrementHeader HeaderAction (step) #:transparent)
(struct DecrementHeader HeaderAction (step) #:transparent)
(struct GotoAction (state) #:transparent)
(struct WriteAction (value) #:transparent)

(define (parse-when-block tokens index-subst)
  (match tokens
    [(list 'WHEN xs ...)
     (match-define (tokens-tuple when-args args-rest) (get-balanced xs 'LPAR 'RPAR))
     (match-define (tokens-tuple when-body rest) (get-balanced args-rest 'LBRA 'RBRA))
     (match-define (list 'STATE (list 'ID id)) when-args)
     (tokens-tuple (WhenBlock (list 'ID (evaluate-index id index-subst))
                              (parse-when-body when-body index-subst)) rest)]))

(define (parse-when-body tokens index-subst [acc '()])
  (match tokens
    [(list 'IF xs ...)
        (match-define (tokens-tuple if-block rest) (parse-when-if xs index-subst))
        (when (and (IfElseBlock? if-block)
                   (or (> 0 (length acc))
                       (> 0 length rest)))
          (error-syntax "`if/else` must be the only statement within a `when` block"))
        (parse-when-body rest index-subst (cons if-block acc))]
    [(list 'FORALL xs ...)
        (match-define (tokens-tuple forall-body rest) (get-balanced xs 'LBRA 'RBRA))
        (when (or (> 0 (length acc))
                (> 0 (length rest)))
              (error-syntax "`forall` must be the only statement within a `when` block"))
        (parse-when-body rest index-subst (cons (Forall (parse-actions forall-body index-subst))
                                                 acc))]
    ['() (reverse acc)]))


(define (parse-when-if tokens index-subst)
  (match-define (tokens-tuple (list 'READ read-value) if-args-rest) (get-balanced tokens 'LPAR 'RPAR))
  (match-define (tokens-tuple if-body if-body-rest) (get-balanced if-args-rest 'LBRA 'RBRA))
  (match if-body-rest
    [(list 'ELSE xs ...)
     (match-define (tokens-tuple else-body else-rest) (get-balanced xs 'LBRA 'RBRA))
     (tokens-tuple (IfElseBlock read-value (parse-actions if-body index-subst)
                                           (parse-actions else-body index-subst)) else-rest)]
    [_ (tokens-tuple (IfBlock read-value (parse-actions if-body index-subst)) if-body-rest)]))

(define (parse-actions tokens index-subst [acc '()])
  (match tokens
    ['() (reverse acc)]
    [(list xs ...)
     (match-define (tokens-tuple action-line rest) (get-semicol-line tokens))
     (let [(action (parse-action action-line index-subst))]
       (if (already-has-action? acc action)
         (error-syntax (format "Action list already has an action of type ~a (duplicate action: ~a)"
                               (object-name action) action))
         (parse-actions rest index-subst (cons action acc))))]))

(define (parse-action action-line index-subst)
  (match action-line
    [(list 'GOTO (list 'ID id)) (GotoAction (list 'ID (evaluate-index id index-subst)))]
    [(list 'GOTO moving-state ...) (GotoAction (list 'ID (evaluate-moving-state moving-state index-subst)))]
    [(list 'WRITE val) (WriteAction val)]
    [(list 'HEADER 'RHEAD step) (IncrementHeader step)]
    [(list 'HEADER 'LHEAD step) (DecrementHeader step)]))

(define (already-has-action? lst action)
  (member action lst (lambda (x y) (equal? (object-name x)
                                           (object-name y)))))


(define (group-by-pairs lst)
  (define (clock-signal n)
    (if (zero? n) '()
      (cons (zero? (modulo (add1 n) 2)) (clock-signal (sub1 n)))))
  (if (> 2 (length lst))
      lst
      (let [(alternating-tuples (map cons lst (reverse (clock-signal (length lst)))))]
        (reverse (foldl (lambda (x acc)
                         (if (cdr x)
                           (cons (car x) acc)
                           (cons (cons (car acc) (car x)) (cdr acc))))
                      '() alternating-tuples)))))

(define (set-index-offset index offset index-subst [acc '()])
  (match index-subst
    [(list (cons some-index value) xs ...)
     (if (equal? some-index index)
       (append (reverse acc) (list (cons index (+ value offset))) xs) ; <- return
       (if (empty? xs)
         (error-index (format "State index ~a not within proper for loop"))
         (set-index-offset index offset xs (cons (cons some-index value)
                                                acc))))]
    ['() (reverse acc)]))

(define (char->symbol c)
  (string->symbol (string c)))

(define (calculate-index-substitutions state-expr-tuples index-subst)
  ; NOT TESTED
  (match state-expr-tuples
    ['() index-subst]
    [(list (cons 'INC (list 'ID id)) xs ...)
     (calculate-index-substitutions xs
                      (set-index-offset (char->symbol (string-ref (~a id) (sub1 (string-length (symbol->string id)))))
                           1 index-subst))]
    [(list (cons 'DEC (list 'ID id)) xs ...)
     (calculate-index-substitutions xs
                      (set-index-offset (char->symbol (string-ref (~a id) (sub1 (string-length (symbol->string id)))))
                           -1 index-subst))]))

(define (odds lst)
  (if (empty? lst)
    '()
    (cons (car lst) (if (cons? (cdr lst))
                      (odds (cddr lst))
                      '()))))


(define (evaluate-moving-state moving-state index-subst)
  (let [(new-index-subst (calculate-index-substitutions (group-by-pairs (reverse moving-state))
                                                         index-subst))
        (state-expr (foldl (lambda (x acc)
                             (string-append acc (symbol->string (cadr x)))) "" (odds moving-state)))]

    (evaluate-string-index state-expr new-index-subst)))

;; AST checkers
(define (check-aliases ast)
  (match-define (CTM-AST _ alias declare _) ast)
  (match-define (States _ _ all-states) declare)
  (let loop ([alias-list alias])
    (unless (empty? alias-list)
      (let ([alias-target (cdr (car alias-list))])
          (when (not (member alias-target all-states))
            (match-define (list 'ID target) alias-target)
            (match-define (list 'ID id-alias) (caar alias-list))
            (error-state (format "Alias ~a references to undeclared ~a state" id-alias target)))
          (loop (cdr alias-list))))))

(define (check-transition-states ast)
  #f)
(define (check-transition-values ast)
  #f)

;; Colophon

(define (lex-and-parse prog)
  (ctm-parser (ctm-lexer prog)))


; Test
(require test-engine/racket-tests)

(check-expect (tokens-tuple-main (get-balanced '{ 1 2 lp 3 4 lp 5 6 rp 7 lp 8 9 rp 10 rp 11 rp 12 } 'lp 'rp))
              '(3 4 lp 5 6 rp 7 lp 8 9 rp 10))
(check-expect (evaluate-index 'qPi (list (cons 'P 4) (cons 'i 2)))
              'q42)
(check-expect (group-by-pairs '(a b c d e f))
              '((a . b) (c . d) (e . f)))
(check-expect (group-by-pairs '(a b c d e))
              '((a . b) (c . d) e))

(define ctm-parser-test (lambda () (test)))
