#lang racket

(provide tokenize token? token-type token-value token-line)

(struct token (type value line) #:transparent)

(define keyword-table
  (hash 'sensor 'SENSOR
        'output 'OUTPUT
        'actuator 'OUTPUT
        'var 'VAR
        'let 'LET
        'rule 'RULE
        'if 'IF
        'else 'ELSE
        'for 'FOR
        'every 'EVERY
        'fn 'FN
        'return 'RETURN
        'machine 'MACHINE
        'state 'STATE
        'init 'INIT
        'unsafe 'UNSAFE
        'invariant 'INVARIANT
        'requires 'REQUIRES
        'ensures 'ENSURES
        'emit 'EMIT
        'extern 'EXTERN
        'initial 'INITIAL
        'transition 'TRANSITION
        'on_enter 'ON_ENTER
        'on_interrupt 'ON_INTERRUPT
        'when 'WHEN
        'after 'AFTER
        'ON 'ON
        'OFF 'OFF
        'fixed 'FIXED
        'to 'TO
        'is 'IS
        'true 'TRUE
        'false 'FALSE
        'and 'AND
        'or 'OR
        'pin 'PIN
        'payload 'PAYLOAD
        'range 'RANGE_KW
        'in 'IN))

(define multi-char-ops
  '(("->" . ARROW) (".." . RANGE) ("<=" . LTE) (">=" . GTE)
    ("==" . EQ) ("!=" . NEQ)))

(define single-char-ops
  (hash #\@ 'AT #\? 'QUESTION #\: 'COLON #\= 'ASSIGN
        #\( 'LPAREN #\) 'RPAREN #\{ 'LBRACE #\} 'RBRACE
        #\[ 'LBRACKET #\] 'RBRACKET #\; 'SEMICOLON #\, 'COMMA
        #\. 'DOT #\+ 'PLUS #\- 'MINUS #\* 'STAR #\/ 'SLASH
        #\% 'PERCENT #\! 'NOT #\< 'LT #\> 'GT #\# 'HASH))

(define (id-start? c) (or (char-alphabetic? c) (char=? c #\_)))
(define (id-char? c) (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))

(define (tokenize str)
  (define len (string-length str))
  (define toks '())
  (define indent-stack (list 0))
  (define at-line-start #t)
  (define line 1)
  
  (define (emit type [val #f])
    (set! toks (cons (token type val line) toks)))
  (define (peek i) (and (< i len) (string-ref str i)))
  (define (peek2 i) (and (< (add1 i) len) (string-ref str (add1 i))))
  
  (let loop ([i 0])
    (when (< i len)
      (define c (string-ref str i))
      (cond
        ;; At line start: measure indentation
        [at-line-start
         (cond
           [(char=? c #\newline) (set! line (add1 line)) (loop (add1 i))]
           [(or (char=? c #\space) (char=? c #\tab) (char=? c #\return)) (loop (add1 i))]
           [else
            (define line-start
              (let l ([j i])
                (cond [(<= j 0) 0]
                      [(char=? (string-ref str (sub1 j)) #\newline) j]
                      [else (l (sub1 j))])))
            (define (count-indent j)
              (cond [(>= j len) 0]
                    [(char=? (string-ref str j) #\space) (+ 1 (count-indent (add1 j)))]
                    [(char=? (string-ref str j) #\tab) (+ 4 (count-indent (add1 j)))]
                    [else 0]))
            (define indent (count-indent line-start))
            (define cur (car indent-stack))
            (cond
              [(> indent cur)
               (set! indent-stack (cons indent indent-stack))
               (emit 'INDENT)]
              [(< indent cur)
               (let drop ()
                 (when (and (pair? (cdr indent-stack))
                            (> (car indent-stack) indent))
                   (set! indent-stack (cdr indent-stack))
                   (emit 'DEDENT)
                   (drop)))])
            (set! at-line-start #f)
            (loop i)])]
        
        ;; Line comment
        [(and (char=? c #\/) (char=? (peek2 i) #\/))
         (let skip ([j i])
           (cond [(>= j len) (void)]
                 [(char=? (string-ref str j) #\newline) (loop j)]
                 [else (skip (add1 j))]))]
        
        ;; Block comment
        [(and (char=? c #\/) (char=? (peek2 i) #\*))
         (let skip ([j (+ i 2)])
           (cond [(>= (add1 j) len) (loop j)]
                 [(and (char=? (string-ref str j) #\*)
                       (char=? (string-ref str (add1 j)) #\/))
                  (loop (+ j 2))]
                 [else (skip (add1 j))]))]
        
        ;; Newline
        [(char=? c #\newline)
         (emit 'NEWLINE)
         (set! line (add1 line))
         (set! at-line-start #t)
         (loop (add1 i))]
        
        ;; Whitespace
        [(or (char=? c #\space) (char=? c #\tab) (char=? c #\return)) (loop (add1 i))]
        
        ;; Hex number
        [(and (char=? c #\0) (char=? (peek2 i) #\x))
         (let read-hx ([j (+ i 2)])
           (cond
             [(>= j len)
              (emit 'INTEGER (string->number (substring str (+ i 2) j) 16))
              (loop j)]
             [(or (char-numeric? (string-ref str j))
                  (char<=? #\a (string-ref str j) #\f)
                  (char<=? #\A (string-ref str j) #\F))
              (read-hx (add1 j))]
             [else
              (emit 'INTEGER (string->number (substring str (+ i 2) j) 16))
              (loop j)]))]
        
        ;; String literal
        [(char=? c #\")
         (let read-str ([j (add1 i)] [acc '()])
           (cond
             [(>= j len) (error 'tokenize "Unterminated string")]
             [(char=? (string-ref str j) #\")
              (emit 'STRING (list->string (reverse acc)))
              (loop (add1 j))]
             [(char=? (string-ref str j) #\\)
              (read-str (+ j 2) (cons (string-ref str (add1 j)) acc))]
             [else (read-str (add1 j) (cons (string-ref str j) acc))]))]
        
        ;; Number
        [(char-numeric? c)
         (let read-num ([j i] [has-dot #f])
           (define (finish k)
             (define numstr (substring str i k))
             (define (check-suffix)
               (cond
                 [(and (<= (+ k 2) len) (string=? (substring str k (+ k 2)) "ms"))
                  (emit 'TIME_MS (string->number numstr))
                  (+ k 2)]
                 [(and (<= (+ k 3) len) (string=? (substring str k (+ k 3)) "min"))
                  (emit 'TIME_MS (* 60000 (string->number numstr)))
                  (+ k 3)]
                 [(and (< k len) (char=? (string-ref str k) #\s)
                       (not (and (< (add1 k) len) (id-char? (string-ref str (add1 k))))))
                  (emit 'TIME_MS (* 1000 (string->number numstr)))
                  (add1 k)]
                 [else
                  (if has-dot
                      (emit 'FLOAT (string->number numstr))
                      (emit 'INTEGER (string->number numstr)))
                  k]))
             (loop (check-suffix)))
           (cond
             [(>= j len) (finish j)]
             [(char-numeric? (string-ref str j)) (read-num (add1 j) has-dot)]
             [(and (not has-dot) (char=? (string-ref str j) #\.)
                   (not (and (< (add1 j) len) (char=? (string-ref str (add1 j)) #\.))))
              (read-num (add1 j) #t)]
             [else (finish j)]))]
        
        ;; Identifier / keyword
        [(id-start? c)
         (let read-id ([j i])
           (cond
             [(>= j len) (void)]
             [(id-char? (string-ref str j)) (read-id (add1 j))]
             [else
              (define s (substring str i j))
              (define kw (hash-ref keyword-table (string->symbol s) #f))
              (if kw (emit kw) (emit 'IDENTIFIER s))
              (loop j)]))]
        
        ;; Operators
        [else
         (define two (and (peek2 i) (string c (peek2 i))))
         (define op2 (and two (assoc two multi-char-ops)))
         (cond
           [op2 (emit (cdr op2)) (loop (+ i 2))]
           [(hash-ref single-char-ops c #f)
            => (lambda (t) (emit t) (loop (add1 i)))]
           [else (error 'tokenize "Unknown character: ~a" c)])])))
  
  (for ([_ (cdr indent-stack)]) (emit 'DEDENT))
  (emit 'EOF)
  (reverse toks))
