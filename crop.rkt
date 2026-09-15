#lang racket

(require "lexer.rkt")
(require "parser.rkt")
(require "codegen.rkt")

(define (compile-crop input-path)
  (define source (file->string input-path))
  (define tokens (tokenize source))
  (define ast (parse-program tokens))
  (define c-path (path-replace-extension input-path #".c"))
  (generate-c ast c-path)
  (printf "Generated: ~a\n" c-path)
  (define exe-path (path-replace-extension input-path #""))
  (define cmd (format "gcc -O1 -o ~a ~a -lm" exe-path c-path))
  (printf "Running: ~a\n" cmd)
  (define rc (system cmd))
  (when (not (zero? rc))
    (eprintf "ERROR: gcc failed\n")
    (exit 1))
  (printf "✓ Compiled: ~a\n" exe-path))

(module+ main
  (define args (current-command-line-arguments))
  (when (= 0 (vector-length args))
    (eprintf "Usage: racket crop.rkt <input.crop>\n")
    (exit 1))
  (compile-crop (vector-ref args 0)))
