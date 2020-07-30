#lang racket

(require "parser.rkt")

(provide ctm-transpiler
         ctm-lex-parse-transpile
         ctm-preprocess)

(define indent 4)
(define start-indent-level 3)
(define (make-indent level)
  (make-string (* (+ level start-indent-level) indent) #\space))

;; Errors
(define (error-state [msg #f])
  (if msg
    (error 'State-error msg)
    (error 'State-error)))

(define (error-setup [msg #f])
  (if msg
    (error 'Setup-error msg)
    (error 'Setup-error)))

;; AST checkers
; check-setup :: CTM-AST -> (void)
(define (check-setup ast)
  (match-define (CTM-AST setup _ _ _) ast)
  ; TODO: check that MAX_TAPE_SIZE > INIT_TAPE_SIZE
  #f)

; check-aliases :: CTM-AST -> (void)
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

; merge-when-same-state :: CTM-AST -> CTM-AST
; Merge all WhenBlocks that have the same state into one
; (cases are put into the into the first matching WhenBlock in the transition list)
(define (merge-when-same-state ast)
  (match-define (CTM-AST S A D transition) ast)
  (define (when-block-has-state? whenblock state)
    (match-define (WhenBlock test-state _) whenblock)
    (equal? test-state state))
  (CTM-AST S A D
      (reverse (foldl (lambda (when-block acc)
                   (match-letrec ([(WhenBlock state cases) when-block]
                                  [idx-whenblock-already-here (index-of acc state when-block-has-state?)])
                     (if idx-whenblock-already-here ; if there was already a WhenBlock with this state
                       (match-let ([(WhenBlock _ previous-cases) (list-ref acc idx-whenblock-already-here)])
                         (list-set acc idx-whenblock-already-here ; append this case list there
                                   (WhenBlock state (append previous-cases cases))))
                       (cons when-block acc)))) '() transition)))) ; otherwise, just add the new WhenBlock

; merge-when-same-read :: CTM-AST -> CTM-AST
; Within a WhenBlock, merge all the when-cases (IfBlock/IfElseBlock/Forall) of the
; same type that read the same value into one.
; (cases are put into the into the first matching when-case in the WhenBlock body)
(define (merge-when-same-read ast)
  (match-define (CTM-AST setup A D transition) ast)
  (define blank-symbol (get-blank-symbol setup))
  ; if "blank_symbol = -1", I want to catch both "if (read blank)" and "if (read -1)"
  (define (implicit-if-blank id)
    (if blank-symbol ; if predefined blank symbol...
      (if (equal? 'BLANK id)
        blank-symbol ; ...translate "blank" to its value
        id)
      id))
  (define (same-value? x y)
    (equal? (implicit-if-blank x) (implicit-if-blank y)))
  ; case-list-has-case? :: IfBlock/IfElseBlock/Forall IfBlock/IfElseBlock/Forall -> boolean
  ; #t iff both cases are the same type and read the same value
  (define (case-list-has-case? test-case when-case)
    (match/values (values test-case when-case)
      [((IfBlock testval _) (IfBlock readval _)) (same-value? testval readval)]
      [((IfElseBlock testval _ _) (IfElseBlock readval _ _)) (same-value? testval readval)]
      [((Forall _) (Forall _)) #t]
      [(else _) #f]))
  (CTM-AST setup A D
      (map (lambda (when-block)
             (match-let ([(WhenBlock state cases) when-block])
               (WhenBlock state
                   (reverse (foldl (lambda (when-case acc) ; when-case: IfBlock/IfElseBlock/Forall
                             (let ([idx-whencase-already-here (index-of acc when-case case-list-has-case?)])
                               (if idx-whencase-already-here
                                 (list-set acc idx-whencase-already-here
                                     (let ([previous-when-case (list-ref acc idx-whencase-already-here)])
                                       (match/values (values previous-when-case when-case)
                                         [((IfBlock val previous-actions) (IfBlock _ new-actions))
                                              (IfBlock val (append previous-actions new-actions))]
                                         [((IfElseBlock val prev-pos-actions prev-neg-actions) (IfElseBlock _ new-pos-actions new-neg-actions))
                                              (IfElseBlock val (append prev-pos-actions new-pos-actions)
                                                               (append prev-neg-actions new-neg-actions))]
                                         [((Forall prev-actions) (Forall new-actions))
                                              (Forall (append prev-actions new-actions))])))
                                 (cons when-case acc))))
                              '() cases))))) transition)))

; check-transition-states :: CTM-AST -> (void)
(define (check-transition-states ast)
  (match-define (CTM-AST S alias declare transition) ast)
  (match-let ([tstates (get-transition-states transition)]
              [alias-states (get-aliases alias)]
              [(States _ _ declare-states) declare])
    (let loop ([ts tstates])
      (match ts
        ['() (void)]
        [(cons state xs)
         (if (not (or (member state alias-states)
                      (member state declare-states)))
           (error-state (format "State ~a in transition references to undeclared state or alias" state))
           (loop xs))]))))

; get-aliases :: List[Cons[A B]] -> List[A]
(define (get-aliases alias)
  (map car alias))

; get-transition-states :: List[WhenBlock] -> List[List['ID id]]
(define (get-transition-states transition)
  (let loop ([ts transition] [acc '()])
    (match ts
      ['() (reverse acc)]
      [(cons (WhenBlock state when-body) xs)
       ; TODO: get GotoAction states
       (loop xs (cons state acc))])))

; check-transition-values :: CTM-AST -> (void)
(define (check-transition-values ast)
  (match-define (CTM-AST setup _ _ transition) ast)
  ; TODO
  #f)

; remove-duplicate-declarations :: CTM-AST -> CTM-AST
(define (remove-duplicate-declarations ast)
  (match-define (CTM-AST S A declare T) ast)
  (match-define (States init finals all) declare)
  (CTM-AST S A (States init (remove-duplicates finals)
               (remove-duplicates all)) T))

; Modifies the `declare` part of the AST, making it a pair
; whose car is the old States struct, and its cdr is a
; dict : State id -> Integer
(define (create-state-mapping ast)
  (match-define (CTM-AST S A declare T) ast)
  (match-define (States _ _ all-states) declare)
  (let ([mapping (make-hash (map cons all-states
                                 (sequence->list (in-range 1 (add1 (length all-states))))))])
    (CTM-AST S A (cons declare mapping) T)))

;; Main transpiler

; ctm-transpiler :: Modified CTM-AST -> List[Cons['macros List[C code string]]
;                                            Cons['program List[C code string]]]
(define (ctm-transpiler ast)
   (let ([ast (ctm-preprocess ast)])
      (let ([macros (generate-preamble-macros ast)]
            [program (generate-transitions ast)])
      (list (cons 'macros macros)
            (cons 'program program)))))

; ctm-preprocess :: CTM-AST -> Modified CTM-AST
(define (ctm-preprocess ast)
  (check-setup ast)
  (check-aliases ast)
  (let ([ast (merge-when-same-state ast)])
    (let ([ast (merge-when-same-read ast)])
          (check-transition-states ast)
          (check-transition-values ast)
          (let ([ast (remove-duplicate-declarations ast)])
            (let ([ast (create-state-mapping ast)])
              ast)))))


; ctm-lex-parse-transpile :: Input port -> List[Cons['macros List[C code string]]
;                                               Cons['program List[C code string]]]
(define (ctm-lex-parse-transpile prog)
  (ctm-transpiler (ctm-lex-and-parse prog)))

;; Misc
; ->C-value :: value/any -> string
(define (->C-value val)
  (match val
    [(list 'INT n) (number->string n)]
    [(list 'CHAR c) (format "'~a'" c)]
    [x (~a x)]))

;; Preamble
(define (generate-preamble-macros ast)
  (match-define (CTM-AST setup alias declare _) ast)
  (match-define (cons _ mapping) declare)
  (let ([setup-macros (generate-macros-setup setup)]
        [alias-macros (generate-macros-alias alias mapping)]
        [declare-macros (generate-macros-declare declare)])
  (append setup-macros
          alias-macros
          declare-macros
          (list "#define __CTM_TRANSPILED_MACROS"))))

;; Preamble from setup
; generate-macros-setup :: setup -> List[C code string]
(define (generate-macros-setup setup)
  (let loop ([st setup] [acc '()])
    (match st
      [(list (Alphabet alphabet-lst) xs ...)
       (loop xs (generate-macros-alphabet alphabet-lst acc))]
      [(list (cons key val) xs ...)
       (loop xs (generate-macro-setup-single-statement key val acc))]
      ['() (reverse acc)])))

; generate-macros-alphabet :: List[value] -> List[C code string]
(define (generate-macros-alphabet alphabet-lst [acc '()])
  (let ([size-line (format "#define CTM_ALPHABET_SIZE ~a" (length alphabet-lst))]
        [alphabet-macro "#define CTM_ALPHABET"]
        [alphabet-line (format "int ctm_alphabet[CTM_ALPHABET_SIZE] = {~a};"
                              (string-join (map ->C-value alphabet-lst) ","))])
    (cons alphabet-line
          (cons alphabet-macro (cons size-line acc)))))

; generate-macros-alphabet :: setup-single-statement value  -> C code string
(define (generate-macro-setup-single-statement key val [acc '()])
  (match key
    ['STP-BLANK_SYMBOL (cons (format "#define CTM_BLANK_SYMBOL ~a" (->C-value val)) acc)]
    ['STP-INIT_TAPE_SIZE (cons (format "#define CTM_INIT_TAPE_SIZE ~a" (->C-value val)) acc)]
    ['STP-MAX_TAPE_SIZE (cons (format "#define CTM_MAX_TAPE_SIZE ~a" (->C-value val)) acc)]
    ['STP-MAX_NUM_TRANSITIONS (cons (format "#define CTM_MAX_NUMBER_TRANSITIONS ~a" (->C-value val)) acc)]
    ['STP-PRINT_STATUS (cons (format "#define CTM_PRINT_STATUS ~a" (->C-value val)) acc)]
    ['STP-ALLOW_PARTIAL_ACCEPT (cons (format "#define CTM_ALLOW_PARTIAL_ACCEPT ~a" (->C-value val)) acc)]
    ['STP-HALT_ON_UNHANDLED_BLANK (cons (format "#define CTM_HALT_ON_UNHANDLED_BLANK ~a" (->C-value val)) acc)]))


;; Preamble from alias
; generate-macros-alias :: alias dict -> List[C code string]
(define (generate-macros-alias alias mapping)
  (map (lambda (alias-pair)
         (match-define (cons (list 'ID alias-id) target) alias-pair)
         (format "#define ~a ~a" alias-id (dict-ref mapping target))) alias))

;; Preamble from declare
; generate-macros-declare :: Cons[declare dict] -> List[C code string]
(define (generate-macros-declare declare)
  (match-define (cons (States init finals all) mapping) declare)
  (let ([number-states-line (format "#define CTM_NUMBER_OF_STATES ~a" (length all))]
        [number-final-states-line (format "#define CTM_NUMBER_OF_FINAL_STATES ~a" (length finals))]
        [final-states-macro "#define CTM_FINAL_STATES"]
        [final-states-line (format "int ctm_final_states[CTM_NUMBER_OF_FINAL_STATES] = {~a};"
                             (string-join (map (lambda (state) (->C-value (dict-ref mapping state))) finals) ","))]
        [init-state-line (format "#define CTM_INIT_STATE ~a" (dict-ref mapping init))])
    (list number-states-line
          number-final-states-line final-states-macro final-states-line
          init-state-line)))

;; Program body
; generate-transitions :: CTM-AST with mapping dict -> List[C code string]
(define (generate-transitions ast)
  (match-define (CTM-AST setup A (cons _ mapping) transition) ast)
  ;(map (lambda (s) (string-append "\t\t\t" s))
       (flatten (map (lambda (t) (generate-transition-cases t mapping setup)) transition)));)

; generate-transition-cases :: WhenBlock dict -> List (of variable nesting) [C code string]
(define (generate-transition-cases when-block mapping setup)
  (match-define (WhenBlock state when-body) when-block)
  (append (list (format "~acase ~a:" (make-indent 0) (dict-ref mapping state (lambda () (unwrap-id state)))) ; case (state)
                (generate-forall-case (filter Forall? when-body) mapping) ; thanks to merge-when-same-read, theres 0 or 1 Forall in the when-body
                (format "~aswitch (TM_read_cell(tm)) {" (make-indent 1)))   ; switch for reading
          (map (lambda (cs) (generate-transition-when-case cs mapping)) (filter (lambda (wb) (not (Forall? wb))) when-body))
          (if (when-body-has-if-block? when-body) ; when-body has no default
            (list (format "~adefault:" (make-indent 2))
                  (format "~areturn TM_state_is_accept(tm);" (make-indent 3)))
            (list ""))
          (if (not (when-body-has-read-blank-symbol? when-body setup)) ; non handled `blank` as stop by default
            (list "#if CTM_HALT_ON_UNHANDLED_BLANK"
                  (format "~acase CTM_BLANK_SYMBOL:" (make-indent 2))
                  (format "~areturn TM_state_is_accept(tm);" (make-indent 3))
                  "#endif // CTM_HALT_ON_UNHANDLED_BLANK")
                  (list ""))
          (list (format "~a}" (make-indent 1))
                (format "~abreak;" (make-indent 0)))))

; generate-transition-when-case :: IfBlock/IfElseBlock/Forall dict -> List[C code string]
(define (generate-transition-when-case when-case mapping)
  (match when-case
    [(IfBlock readvalue actions) ; There might be multiple IfBlock within this WhenBlock
     (append (list (format "~acase ~a:" (make-indent 2) (get-read-value readvalue)))
             (map (lambda (a) (generate-action a mapping)) actions)
             (list (format "~abreak;" (make-indent 3))))]
    [(IfElseBlock readvalue actions elseactions) ; This is the only block within this WhenBlock
     (append (list (format "~acase ~a:" (make-indent 2) (get-read-value readvalue)))
                 (map (lambda (a) (generate-action a mapping)) actions)
                 (list (format "~abreak;" (make-indent 3)))
             (list (format "~adefault:" (make-indent 2)))
                 (map (lambda (a) (generate-action a mapping)) elseactions)
                 (list (format "~abreak;" (make-indent 3))))]
    [(Forall actions)
     (error "This shouldn't happen")
     #;(append (list (format "~adefault:" (make-indent 2)))
             (map (lambda (a) (generate-action a mapping)) actions)
             (list (format "~abreak;" (make-indent 3))))]))

(define (generate-forall-case forall-lst mapping)
  (match forall-lst
    ['() ""]
    [(list (Forall actions))
        (map (lambda (a) (generate-action a mapping -2)) actions)]))

(define (generate-action action mapping [indent-offset 0])
  (match action
    [(GotoAction next) (format "~atm->current_state = ~a;" (make-indent (+ 3 indent-offset)) (dict-ref mapping next (lambda () (unwrap-id next))))]
    [(IncrementHeader step) (format "~aHANDLE_HEADER(tm, TM_move_header_right(tm, ~a));" (make-indent (+ 3 indent-offset)) (->C-value step))]
    [(DecrementHeader step) (format "~aHANDLE_HEADER(tm, TM_move_header_left(tm, ~a));" (make-indent (+ 3 indent-offset)) (->C-value step))]
    [(WriteAction value) (format "~aTM_write_cell(tm, ~a);" (make-indent (+ 3 indent-offset)) (->C-value value))]))

(define (get-read-value value)
  (match value
    ['BLANK "CTM_BLANK_SYMBOL"]
    [x (->C-value x)]))

(define (unwrap-id state)
  (match state
    [(list 'ID id) id]))

(define (when-body-has-if-block? when-body)
  (match when-body
    ['() #f]
    [(list (IfBlock _ _) xs ...) #t]
    [(list _ xs ...) (when-body-has-if-block? xs)]))

; when-body-has-read-value? :: List[IfBlock/IfElseBlock/Forall] Value -> boolean
(define (when-body-has-read-value? when-body val)
  (match when-body
    ['() #f]
    [(list (IfBlock vl _) xs ...)
        (if (equal? val vl)
         #t
         (when-body-has-read-value? xs val))]
    [(list (IfElseBlock vl _ _) xs ...)
        (if (equal? val vl)
         #t
         (when-body-has-read-value? xs val))]
    [(list (Forall _)xs ...) (when-body-has-read-value? xs val)]))

; setup-lookup :: symbol setup -> Value
; like cdr assoc, but doesn't raise "non-pair found in list"
(define (setup-lookup key setup)
  (match setup
    ['()    #f]
    [(list (cons k v) xs ...)
     (if (equal? key k)
       v
       (setup-lookup key xs))]
    [(cons _ xs) (setup-lookup key xs)])) ; not a tuple, keep going

; get-blank-symbol :: setup -> Value/#f
(define (get-blank-symbol setup)
  (let ([blank-entry (setup-lookup 'STP-BLANK_SYMBOL setup)])
    (if blank-entry
      blank-entry
      #f)))

; when-body-has-read-blank-symbol? :: List[IfBlock/IfElseBlock/Forall] setup -> boolean
(define (when-body-has-read-blank-symbol? when-body setup)
  (let ([blank-symbol (get-blank-symbol setup)]
        [has-explicit-blank (when-body-has-read-value? when-body 'BLANK)])
    (if blank-symbol
      (or (when-body-has-read-value? when-body blank-symbol)
          has-explicit-blank)
      has-explicit-blank)))
