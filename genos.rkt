#lang racket
(provide #| show Literal ch-list->prog nfun! leave-scope!
         test0 test1 |# (all-defined-out))

; this is a simple implementation of what will be compiled to the Genos bytecode.
; *MUTABLE-VALUE*

(struct Literal (val type))
(struct Function (name in out))

; *FLIST* :: [Function | SCOPE]
;   new functions are cons'ed in.
(define *FLIST* (list 'scope))

(define b-app bytes-append)
; notation note: /= works for all types despite similar notation to `='.
(define (/= a b) (not (equal? a b)))

(define (compose a b) (lambda (x) (a (b x))))
(define (uncurry a b) (lambda (x) (a b x)))
(define uc uncurry)

(define (splitf-at2 lst expr) (list (takef lst expr) (dropf lst expr)))

(define (init- lst) (reverse (cdr (reverse lst))))

(define (<< . a) (case (cdr a)
  [(()) a] [else (compose (car a) (<< . (cdr a)))]))

(define (cons-lit a b) (if (string? (car b)) (cons `(,a) b) (cons (cons a (car b)) (cdr b))))

; for now: int32, float64, bint32, bfloat64

; show :: Literal -> ByteString
;   encodes in little-endian.
(define (show l)
  (case (Literal-type l)
    [(int32) (b-app (bytes 0) (integer->integer-bytes (Literal-val l) 4 #f))]
    [(float64) (b-app (bytes 1) (real->floating-point-bytes (Literal-val l) 8))]
    [(byte) (Literal-val l)]
    [(generator) (bytes-append* (map show 
                   (cons (Literal (bytes 26 #| Generator |#) 'byte)
                     (cons (Literal (integer->integer-bytes (length (Literal-val l)) 4 #f) 'byte)
                         (Literal-val l)))))]
    [else #"Error"]))

(define (nfun! name in out) (set! *FLIST* (cons (Function name in out) *FLIST*)))
(define (leave-scope! name in out) (set! *FLIST* (cdr (dropf *FLIST* (lambda (x) (/= x 'scope))))))

; TODO: define `split' for expression of whitespace or ([{}]). splif-at recursive/fold.
;   [] is unscoped expr, {} is scoped expr.

; string->prog :: [String] -> (x :: [Literal | x])
(define (ch-list->prog lst) (foldr (lambda (x n) (cond
  [(char-whitespace? x) (if (empty? (car n)) n (cons '() n))]
  [(member x '(#\) #\} #\] #\, #\;)) (cons (string x) (cdr n) #| inspect |#)]
  [(member x '(#\( #\{ #\[)) ;(begin (displayln
    ;(splitf-at2 n (uc /= (case x [(#\() ")"] [(#\[) "]"] [(#\{) "}"]))))
    ((match-lambda [(list y z) (cons y (cdr z))])
      (splitf-at2 n (uc /= (case x [(#\() ")"] [(#\[) "]"] [(#\{) "}"]))))]
  [else (cons-lit x n)])) '(()) lst))

(define test1 (ch-list->prog (string->list "ab bc cd")))
(define test0 (Literal (list (Literal 1 'int32)) 'generator))
(define test2 (ch-list->prog (string->list "ab (cd ef)")))

; parsing rules:
;   take until a non-literal or symbol is found.
;     single literal    -> that literal;
;     multiple literals -> generator of those literals
