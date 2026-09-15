#lang racket

(require "lexer.rkt")
(require "ast.rkt")

(provide parse-program)

(define tokens (make-parameter '()))

(define (peek) (car (tokens)))
(define (peek-type) (token-type (peek)))
(define (peek-value) (token-value (peek)))
(define (advance!) (tokens (cdr (tokens))))

(define (expect type)
  (when (not (eq? (peek-type) type))
    (error 'parse "Line ~a: expected ~a, got ~a"
           (token-line (peek)) type (peek-type)))
  (define t (peek))
  (advance!)
  t)

(define (skip-newlines!)
  (let loop ()
    (when (eq? (peek-type) 'NEWLINE) (advance!) (loop))))

;; === Program ===
(define (parse-program toks)
  (parameterize ([tokens toks])
    (let loop ([decls '()])
      (case (peek-type)
        [(NEWLINE) (advance!) (loop decls)]
        [(EOF) (program (reverse decls))]
        [else (loop (cons (parse-declaration) decls))]))))

(define (parse-declaration)
  (case (peek-type)
    [(SENSOR) (parse-sensor)]
    [(OUTPUT) (parse-output)]
    [(VAR) (parse-var)]
    [(LET) (parse-let)]
    [(RULE) (parse-rule)]
    [(EVERY) (parse-every)]
    [(PAYLOAD) (parse-payload)]
    [(FN) (parse-fn)]
    [(MACHINE) (parse-machine)]
    [(INVARIANT) (parse-invariant)]
    [(EMIT) (parse-emit)]
    [(INIT) (parse-init)]
    [(EXTERN) (parse-extern)]
    [(UNSAFE) (parse-unsafe)]
    [(ON_INTERRUPT) (parse-on-interrupt)]
    [else (error 'parse "Line ~a: unexpected token ~a"
                 (token-line (peek)) (peek-type))]))

;; === Declarations ===
(define (parse-sensor)
  (expect 'SENSOR)
  (define name (token-value (expect 'IDENTIFIER)))
  (if (eq? (peek-type) 'PIN)
      (begin
        (advance!)
        (define pin (token-value (expect 'IDENTIFIER)))
        (define range (parse-opt-range))
        (define poll (parse-opt-poll))
        (expect 'NEWLINE)
        (sensor-decl name 'pin pin #f #f #f poll range))
      (begin
        (define protocol (token-value (expect 'IDENTIFIER)))
        (expect 'LPAREN)
        (define addr (parse-expr))
        (expect 'RPAREN)
        (expect 'ARROW)
        (define type (token-value (expect 'IDENTIFIER)))
        (define poll (parse-opt-poll))
        (expect 'NEWLINE)
        (sensor-decl name 'bus #f protocol addr type poll #f))))

(define (parse-opt-range)
  (if (eq? (peek-type) 'RANGE_KW)
      (begin
        (advance!)
        (define lo (parse-expr))
        (expect 'RANGE)
        (define hi (parse-expr))
        (range-expr lo hi))
      #f))

(define (parse-opt-poll)
  (if (eq? (peek-type) 'AT)
      (begin
        (advance!)
        (expect 'IDENTIFIER)
        (expect 'ASSIGN)
        (parse-expr))
      #f))

(define (parse-output)
  (expect 'OUTPUT)
  (define name (token-value (expect 'IDENTIFIER)))
  (expect 'PIN)
  (define pin (token-value (expect 'IDENTIFIER)))
  (define initial
    (case (peek-type)
      [(ON) (advance!) "ON"]
      [(OFF) (advance!) "OFF"]
      [else "OFF"]))
  (expect 'NEWLINE)
  (output-decl name pin initial))

(define (parse-var)
  (expect 'VAR)
  (define name (token-value (expect 'IDENTIFIER)))
  (expect 'COLON)
  (define type (token-value (expect 'IDENTIFIER)))
  (expect 'ASSIGN)
  (define value (parse-expr))
  (expect 'NEWLINE)
  (var-decl name type value))

(define (parse-let)
  (expect 'LET)
  (define name (token-value (expect 'IDENTIFIER)))
  (expect 'ASSIGN)
  (define value (parse-expr))
  (expect 'NEWLINE)
  (let-decl name value))

(define (parse-rule)
  (expect 'RULE)
  (define name (token-value (expect 'STRING)))
  (expect 'COLON)
  (expect 'NEWLINE)
  (define body (parse-block))
  (rule-decl name body))

(define (parse-every)
  (expect 'EVERY)
  (define interval (parse-expr))
  (expect 'COLON)
  (expect 'NEWLINE)
  (define body (parse-block))
  (every-decl interval body))

(define (parse-payload)
  (expect 'PAYLOAD)
  (define name (token-value (expect 'IDENTIFIER)))
  (expect 'COLON)
  (expect 'NEWLINE)
  (expect 'INDENT)
  (let loop ([fields '()])
    (cond
      [(eq? (peek-type) 'DEDENT)
       (advance!)
       (expect 'NEWLINE)
       (payload-decl name (reverse fields))]
      [(eq? (peek-type) 'NEWLINE) (advance!) (loop fields)]
      [else
       (define type (token-value (expect 'IDENTIFIER)))
       (define fname (token-value (expect 'IDENTIFIER)))
       (expect 'NEWLINE)
       (loop (cons (payload-field type fname) fields))])))

(define (parse-fn)
  (expect 'FN)
  (define name (token-value (expect 'IDENTIFIER)))
  (expect 'LPAREN)
  (define params (parse-param-list))
  (expect 'RPAREN)
  (expect 'ARROW)
  (define ret (token-value (expect 'IDENTIFIER)))
  (expect 'COLON)
  (expect 'NEWLINE)
  (define body (parse-block))
  (fn-decl name params ret body))

(define (parse-param-list)
  (cond
    [(eq? (peek-type) 'RPAREN) '()]
    [else
     (define name (token-value (expect 'IDENTIFIER)))
     (expect 'COLON)
     (define type (token-value (expect 'IDENTIFIER)))
     (let loop ([ps (list (param name type))])
       (if (eq? (peek-type) 'COMMA)
           (begin
             (advance!)
             (define n (token-value (expect 'IDENTIFIER)))
             (expect 'COLON)
             (define t (token-value (expect 'IDENTIFIER)))
             (loop (cons (param n t) ps)))
           (reverse ps)))]))

(define (parse-machine)
  (expect 'MACHINE)
  (define name (token-value (expect 'IDENTIFIER)))
  (expect 'COLON)
  (expect 'NEWLINE)
  (define body (parse-block))
  (machine-decl name body))

(define (parse-invariant)
  (expect 'INVARIANT)
  (define name (token-value (expect 'STRING)))
  (expect 'COLON)
  (define cond (parse-expr))
  (expect 'NEWLINE)
  (invariant-decl name cond))

(define (parse-emit)
  (expect 'EMIT)
  (define payload (token-value (expect 'IDENTIFIER)))
  (expect 'LPAREN)
  (define args (parse-arg-list))
  (expect 'RPAREN)
  (expect 'TO)
  (define proto (token-value (expect 'IDENTIFIER)))
  (expect 'LPAREN)
  (define addr (token-value (expect 'STRING)))
  (expect 'RPAREN)
  (expect 'NEWLINE)
  (emit-decl payload args proto addr))

(define (parse-init)
  (expect 'INIT)
  (expect 'COLON)
  (expect 'NEWLINE)
  (define body (parse-block))
  (init-decl body))

(define (parse-extern)
  (expect 'EXTERN)
  (define lang (token-value (expect 'STRING)))
  (expect 'FN)
  (define name (token-value (expect 'IDENTIFIER)))
  (expect 'LPAREN)
  (define params (parse-param-list))
  (expect 'RPAREN)
  (expect 'ARROW)
  (define ret (token-value (expect 'IDENTIFIER)))
  (expect 'NEWLINE)
  (extern-decl lang name params ret))

(define (parse-unsafe)
  (expect 'UNSAFE)
  (expect 'LBRACE)
  (define code (token-value (expect 'STRING)))
  (expect 'RBRACE)
  (expect 'NEWLINE)
  (unsafe-block code))

(define (parse-on-interrupt)
  (expect 'ON_INTERRUPT)
  (define pin (token-value (expect 'IDENTIFIER)))
  (define edge (token-value (expect 'IDENTIFIER)))
  (expect 'COLON)
  (expect 'NEWLINE)
  (define body (parse-block))
  (on-interrupt-decl pin edge body))

;; === Blocks ===
(define (parse-block)
  (expect 'INDENT)
  (let loop ([stmts '()])
    (cond
      [(eq? (peek-type) 'DEDENT)
       (advance!)
       (reverse stmts)]
      [(eq? (peek-type) 'NEWLINE) (advance!) (loop stmts)]
      [else (loop (cons (parse-statement) stmts))])))

(define (parse-statement)
  (define s
    (case (peek-type)
      [(IF) (parse-if)]
      [(RETURN)
       (advance!)
       (define v (if (eq? (peek-type) 'NEWLINE) #f (parse-expr)))
       (return-stmt v)]
      [(LET) (parse-let)]
      [else (parse-simple-stmt)]))
  (when (eq? (peek-type) 'NEWLINE) (advance!))
  s)

(define (parse-if)
  (expect 'IF)
  (define cond (parse-expr))
  (expect 'COLON)
  (expect 'NEWLINE)
  (define then (parse-block))
  (define else
    (if (eq? (peek-type) 'ELSE)
        (begin
          (advance!)
          (expect 'COLON)
          (expect 'NEWLINE)
          (parse-block))
        '()))
  (if-stmt cond then else))

(define (parse-simple-stmt)
  (define lhs (parse-expr))
  (if (eq? (peek-type) 'ASSIGN)
      (begin
        (advance!)
        (define rhs (parse-expr))
        (assign-stmt lhs rhs))
      lhs))

;; === Expressions ===
(define (parse-expr) (parse-cmp))

(define (parse-cmp)
  (let loop ([l (parse-and)])
    (case (peek-type)
      [(LT GT LTE GTE EQ NEQ)
       (define op (peek-type))
       (advance!)
       (loop (binop op l (parse-and)))]
      [else l])))

(define (parse-and)
  (let loop ([l (parse-add)])
    (case (peek-type)
      [(AND)
       (advance!)
       (loop (binop 'AND l (parse-add)))]
      [else l])))

(define (parse-add)
  (let loop ([l (parse-mul)])
    (case (peek-type)
      [(PLUS MINUS)
       (define op (peek-type))
       (advance!)
       (loop (binop op l (parse-mul)))]
      [else l])))

(define (parse-mul)
  (let loop ([l (parse-unary)])
    (case (peek-type)
      [(STAR SLASH PERCENT)
       (define op (peek-type))
       (advance!)
       (loop (binop op l (parse-unary)))]
      [else l])))

(define (parse-unary)
  (case (peek-type)
    [(NOT) (advance!) (unop 'NOT (parse-unary))]
    [(MINUS) (advance!) (unop 'NEG (parse-unary))]
    [else (parse-postfix)]))

(define (parse-postfix)
  (let loop ([b (parse-primary)])
    (case (peek-type)
      [(QUESTION)
       (advance!)
       (loop (sensor-read (identifier-name b)))]
      [(IS)
       (advance!)
       (define state
         (case (peek-type)
           [(ON) (advance!) "ON"]
           [(OFF) (advance!) "OFF"]
           [(IDENTIFIER) (define n (token-value (peek))) (advance!) n]
           [else (error 'parse "Expected ON/OFF/identifier after is")]))
       (loop (is-expr b state))]
      [else b])))

(define (parse-primary)
  (case (peek-type)
    [(INTEGER) (define v (token-value (peek))) (advance!) (integer-lit v)]
    [(FLOAT) (define v (token-value (peek))) (advance!) (float-lit v)]
    [(TIME_MS) (define v (token-value (peek))) (advance!) (integer-lit v)]
    [(TRUE) (advance!) (boolean-lit #t)]
    [(FALSE) (advance!) (boolean-lit #f)]
    [(STRING) (define v (token-value (peek))) (advance!) (string-lit v)]
    [(ON) (advance!) (state-lit "ON")]
    [(OFF) (advance!) (state-lit "OFF")]
    [(IDENTIFIER)
     (define name (token-value (peek)))
     (advance!)
     (if (eq? (peek-type) 'LPAREN)
         (begin
           (advance!)
           (define args (parse-arg-list))
           (expect 'RPAREN)
           (funcall name args))
         (identifier name))]
    [(LPAREN)
     (advance!)
     (define e (parse-expr))
     (expect 'RPAREN)
     e]
    [else (error 'parse "Line ~a: expected expression, got ~a"
                 (token-line (peek)) (peek-type))]))

(define (parse-arg-list)
  (cond
    [(eq? (peek-type) 'RPAREN) '()]
    [else
     (define first (parse-expr))
     (let loop ([args (list first)])
       (if (eq? (peek-type) 'COMMA)
           (begin (advance!) (loop (cons (parse-expr) args)))
           (reverse args)))]))
