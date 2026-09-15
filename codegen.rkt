#lang racket

(require "ast.rkt")
(require "lexer.rkt")

(provide generate-c)

;; === Helpers ===
(define (sanitize s)
  (list->string
   (for/list ([c (string->list s)])
     (if (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_))
         c #\_))))

(define (c-type t)
  (case (string->symbol t)
    [(u8) "uint8_t"] [(u16) "uint16_t"] [(u32) "uint32_t"] [(u64) "uint64_t"]
    [(i8) "int8_t"] [(i16) "int16_t"] [(i32) "int32_t"] [(i64) "int64_t"]
    [(f32) "float"] [(f64) "double"] [(bool) "bool"]
    [else "void"]))

(define (build-symbols decls)
  (define tbl (make-hash))
  (for ([d decls])
    (cond
      [(sensor-decl? d) (hash-set! tbl (sensor-decl-name d) 'sensor)]
      [(output-decl? d) (hash-set! tbl (output-decl-name d) 'output)]
      [(var-decl? d) (hash-set! tbl (var-decl-name d) 'var)]))
  tbl)

;; === Expression codegen ===
(define (emit-expr out e syms)
  (cond
    [(integer-lit? e) (fprintf out "~a" (integer-lit-value e))]
    [(float-lit? e) (fprintf out "~a" (float-lit-value e))]
    [(boolean-lit? e) (fprintf out "~a" (if (boolean-lit-value e) "true" "false"))]
    [(string-lit? e) (fprintf out "\"~a\"" (string-lit-value e))]
    [(state-lit? e)
     (fprintf out "~a" (if (string=? (state-lit-name e) "ON") "true" "false"))]
    [(identifier? e)
     (define n (identifier-name e))
     (case (hash-ref syms n 'var)
       [(output) (fprintf out "output_~a_get()" (sanitize n))]
       [(sensor) (fprintf out "sensor_~a_read_opt()" (sanitize n))]
       [else (fprintf out "~a" (sanitize n))])]
    [(sensor-read? e)
     (fprintf out "sensor_~a_read_opt()" (sanitize (sensor-read-name e)))]
    [(is-expr? e)
     (define t (is-expr-target e))
     (define s (is-expr-state e))
     (if (identifier? t)
         (fprintf out "output_~a_get() == ~a"
                  (sanitize (identifier-name t))
                  (if (string=? s "ON") "true" "false"))
         (error 'codegen "is-expr target must be identifier"))]
    [(binop? e)
     (fprintf out "(")
     (emit-expr out (binop-left e) syms)
     (define op (binop-op e))
     (fprintf out " ~a "
              (case op
                [(PLUS) "+"] [(MINUS) "-"] [(STAR) "*"] [(SLASH) "/"]
                [(PERCENT) "%"] [(LT) "<"] [(GT) ">"] [(LTE) "<="] [(GTE) ">="]
                [(EQ) "=="] [(NEQ) "!="] [(AND) "&&"] [(OR) "||"]
                [else (error 'codegen "unknown op ~a" op)]))
     (emit-expr out (binop-right e) syms)
     (fprintf out ")")]
    [(unop? e)
     (case (unop-op e)
       [(NOT) (fprintf out "!") (emit-expr out (unop-operand e) syms)]
       [(NEG) (fprintf out "-") (emit-expr out (unop-operand e) syms)])]
    [(funcall? e)
     (fprintf out "~a(" (sanitize (funcall-name e)))
     (define args (funcall-args e))
     (for ([a args] [i (in-naturals)])
       (when (> i 0) (fprintf out ", "))
       (emit-expr out a syms))
     (fprintf out ")")]
    [else (error 'codegen "unknown expr: ~a" e)]))

;; === Statement codegen ===
(define (emit-stmt out s syms)
  (cond
    [(assign-stmt? s)
     (define lhs (assign-stmt-lhs s))
     (define rhs (assign-stmt-rhs s))
     (cond
       [(and (identifier? lhs)
             (eq? (hash-ref syms (identifier-name lhs) 'var) 'output))
        (fprintf out "  output_~a_set(" (sanitize (identifier-name lhs)))
        (emit-expr out rhs syms)
        (fprintf out ");\n")]
       [else
        (fprintf out "  ")
        (emit-expr out lhs syms)
        (fprintf out " = ")
        (emit-expr out rhs syms)
        (fprintf out ";\n")])]
    [(return-stmt? s)
     (fprintf out "  return")
     (when (return-stmt-value s)
       (fprintf out " ")
       (emit-expr out (return-stmt-value s) syms))
     (fprintf out ";\n")]
    [(if-stmt? s)
     (fprintf out "  if (")
     (emit-expr out (if-stmt-cond s) syms)
     (fprintf out ") {\n")
     (for ([st (if-stmt-then s)]) (emit-stmt out st syms))
     (fprintf out "  }")
     (when (pair? (if-stmt-else s))
       (fprintf out " else {\n")
       (for ([st (if-stmt-else s)]) (emit-stmt out st syms))
       (fprintf out "  }"))
     (fprintf out "\n")]
    [(let-decl? s)
     ;; Emit nothing; let is not supported in this simplified codegen
     (void)]
    [else
     ;; Bare expression statement (e.g. function call)
     (fprintf out "  ")
     (emit-expr out s syms)
     (fprintf out ";\n")]))

;; === Declaration codegen ===
(define (emit-sensor out d syms)
  (define name (sensor-decl-name d))
  (define safe (sanitize name))
  (when (eq? (sensor-decl-mode d) 'pin)
    (define pin (sensor-decl-pin d))
    (define poll (sensor-decl-poll d))
    (define poll-ms (if poll (integer-lit-value poll) 100))
    (define pin-is-analog
      (and (> (string-length pin) 0)
           (or (char=? (string-ref pin 0) #\A)
               (char=? (string-ref pin 0) #\a))))
    (fprintf out "// Sensor ~a on pin ~a\n" name pin)
    (fprintf out "static bool sensor_~a_valid = false;\n" safe)
    (fprintf out "static ~a sensor_~a_value = 0;\n"
             (if pin-is-analog "uint16_t" "bool") safe)
    (fprintf out "static unsigned long sensor_~a_last_read = 0;\n" safe)
    (fprintf out "void sensor_~a_update() {\n" safe)
    (fprintf out "  if (millis() - sensor_~a_last_read >= ~a) {\n" safe poll-ms)
    (fprintf out "    sensor_~a_last_read = millis();\n" safe)
    (fprintf out "    sensor_~a_value = ~a(~a);\n" safe
             (if pin-is-analog "analogRead" "digitalRead") pin)
    (fprintf out "    sensor_~a_valid = true;\n" safe)
    (fprintf out "  }\n}\n\n")
    (fprintf out "__attribute__((weak)) ~a sensor_~a_read_opt(void) {\n"
             (if pin-is-analog "uint16_t" "bool") safe)
    (fprintf out "  sensor_~a_update();\n" safe)
    (fprintf out "  if (!sensor_~a_valid) return ~a;\n" safe
             (if pin-is-analog "0" "false"))
    (fprintf out "  sensor_~a_valid = false;\n" safe)
    (fprintf out "  return sensor_~a_value;\n}\n\n" safe)))

(define (emit-output out d syms)
  (define name (output-decl-name d))
  (define safe (sanitize name))
  (define pin (output-decl-pin d))
  (define init (if (string=? (output-decl-initial d) "ON") "true" "false"))
  (fprintf out "// Output ~a on pin ~a\n" name pin)
  (fprintf out "static bool output_~a_state = ~a;\n" safe init)
  (fprintf out "void output_~a_set(bool state) {\n" safe)
  (fprintf out "  output_~a_state = state;\n" safe)
  (fprintf out "  digitalWrite(~a, state ? HIGH : LOW);\n}\n\n" pin)
  (fprintf out "bool output_~a_get(void) {\n  return output_~a_state;\n}\n\n" safe safe))

(define (emit-var out d syms)
  (define name (var-decl-name d))
  (define type (c-type (var-decl-type d)))
  (fprintf out "static ~a ~a = " type (sanitize name))
  (emit-expr out (var-decl-value d) syms)
  (fprintf out ";\n\n"))

(define (emit-rule out d syms)
  (define name (rule-decl-name d))
  (define safe (sanitize name))
  (fprintf out "void rule_~a(void) {\n" safe)
  (for ([s (rule-decl-body d)]) (emit-stmt out s syms))
  (fprintf out "}\n\n"))

(define (emit-every out d syms counter)
  (define id (unbox counter))
  (set-box! counter (add1 id))
  (define interval (integer-lit-value (every-decl-interval d)))
  (fprintf out "static unsigned long every_~a_last = 0;\n" id)
  (fprintf out "void every_~a(void) {\n" id)
  (fprintf out "  if (millis() - every_~a_last >= ~a) {\n" id interval)
  (fprintf out "    every_~a_last = millis();\n" id)
  (for ([s (every-decl-body d)])
    ;; indent body statements an extra level - we hack by prefixing "  "
    (define tmp (open-output-string))
    (emit-stmt tmp s syms)
    (define body-str (get-output-string tmp))
    (fprintf out "  ~a" body-str))
  (fprintf out "  }\n}\n\n"))

(define (emit-payload out d syms)
  ;; Simplified: skip payloads in codegen for now
  (void))

(define (emit-invariant out d syms)
  (define name (sanitize (invariant-decl-name d)))
  (fprintf out "static bool invariant_~a(void) { return " name)
  (emit-expr out (invariant-decl-cond d) syms)
  (fprintf out ";\n}\n\n"))

(define (emit-on-interrupt out d syms)
  (define pin (on-interrupt-decl-pin d))
  (define edge (on-interrupt-decl-edge d))
  (fprintf out "void on_interrupt_~a(void) {\n" (sanitize pin))
  (for ([s (on-interrupt-decl-body d)]) (emit-stmt out s syms))
  (fprintf out "}\n\n"))

(define (emit-decl out d syms counter)
  (cond
    [(sensor-decl? d) (emit-sensor out d syms)]
    [(output-decl? d) (emit-output out d syms)]
    [(var-decl? d) (emit-var out d syms)]
    [(rule-decl? d) (emit-rule out d syms)]
    [(every-decl? d) (emit-every out d syms counter)]
    [(payload-decl? d) (emit-payload out d syms)]
    [(invariant-decl? d) (emit-invariant out d syms)]
    [(on-interrupt-decl? d) (emit-on-interrupt out d syms)]
    [else (void)]))

;; === Top-level entry point ===
(define (generate-c ast output-path)
  (define out (open-output-file output-path #:exists 'replace))
  (define decls (program-declarations ast))
  (define syms (build-symbols decls))
  (define every-count (box 0))
  
  (fprintf out "// Generated by CROP Compiler (Racket) V1.0\n")
  (fprintf out "#include <stdint.h>\n#include <stdbool.h>\n#include <stdlib.h>\n#include <string.h>\n#include <math.h>\n\n")
  (fprintf out "#ifdef ARDUINO\n#include <Arduino.h>\n#else\n")
  (fprintf out "#define D0 0\n#define D1 1\n#define D2 2\n#define D3 3\n#define D4 4\n#define D5 5\n")
  (fprintf out "#define D6 6\n#define D7 7\n#define D8 8\n#define D9 9\n#define D10 10\n#define D11 11\n#define D12 12\n#define D13 13\n")
  (fprintf out "#define A0 14\n#define A1 15\n#define A2 16\n#define A3 17\n#define A4 18\n#define A5 19\n")
  (fprintf out "typedef uint16_t pin_t;\n")
  (fprintf out "static unsigned long millis_counter = 0;\n")
  (fprintf out "__attribute__((weak)) unsigned long millis() { return millis_counter; }\n")
  (fprintf out "__attribute__((weak)) void delay(unsigned long ms) {}\n")
  (fprintf out "#define OUTPUT 1\n#define INPUT 0\n#define INPUT_PULLUP 2\n")
  (fprintf out "#define LOW 0\n#define HIGH 1\n#define RISING 1\n#define FALLING 0\n#define CHANGE 2\n")
  (fprintf out "__attribute__((weak)) void pinMode(pin_t p, int m) {}\n")
  (fprintf out "__attribute__((weak)) void digitalWrite(pin_t p, int v) {}\n")
  (fprintf out "__attribute__((weak)) int digitalRead(pin_t p) { return 0; }\n")
  (fprintf out "__attribute__((weak)) int analogRead(pin_t p) { return 512; }\n")
  (fprintf out "__attribute__((weak)) void attachInterrupt(pin_t p, void (*fn)(), int m) {}\n")
  (fprintf out "#endif\n\n")
  
  ;; Emit all declarations
  (for ([d decls]) (emit-decl out d syms every-count))
  
  ;; Emit main loop
  (define every-n (unbox every-count))
  (fprintf out "__attribute__((weak)) void setup(void) {}\n\n")
  (fprintf out "__attribute__((weak)) void loop(void) {\n")
  (for ([i (in-range every-n)]) (fprintf out "  every_~a();\n" i))
  (for ([d decls] #:when (rule-decl? d))
    (fprintf out "  rule_~a();\n" (sanitize (rule-decl-name d))))
  (fprintf out "}\n\n")
  (fprintf out "#ifndef ARDUINO\n")
  (fprintf out "__attribute__((weak)) int main(void) {\n")
  (fprintf out "  setup();\n")
  (fprintf out "  while (1) { loop(); millis_counter++; }\n")
  (fprintf out "  return 0;\n}\n")
  (fprintf out "#endif\n")
  
  (close-output-port out))
