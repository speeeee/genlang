#lang racket
(provide #| show Literal ch-list->prog nfun! leave-scope!
         test0 test1 |# (all-defined-out))

; this is a simple implementation of what will be compiled to the Genos bytecode.
; *MUTABLE-VALUE*

(struct Literal (val type) #:transparent)
(struct Function (name ind in out runtime) #:transparent)
; First, whatever is in runtime is run (could be id for non-runtime functions).

(define (id x) x)

; *FLIST* :: [Function | SCOPE]
;   new functions are cons'ed in.
(define *FLIST* (list (Function "+" 1 '(int32 int32) 'int32 id) 'scope))
(define *OUTPUT* '())

(define b-app bytes-append)
; notation note: /= works for all types despite similar notation to `='.
(define (/= a b) (not (equal? a b)))

(define (compose a b) (lambda (x) (a (b x))))
(define (uncurry a b) (lambda (x) (a b x)))
(define uc uncurry)

(define (flip a) (lambda (x y) (a y x)))
(define (fork a f g) (lambda (x) (a (f x) (g x))))

(define (splitf-at2 lst expr) (list (takef lst expr) (dropf lst expr)))

(define (init- lst) (reverse (cdr (reverse lst))))

(define (<< . a) (case (cdr a)
  [(()) a] [else (compose (car a) (<< . (cdr a)))]))
; similar but not the same as Haskell's version of >>=
(define (>>= a b) (if a (b a) 'nothing))

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

(define (f-find-and-apply sd . n)
  (>>= (findf *FLIST* (lambda (x) (and (Function? x) (equal? (Function-name x) sd))))
       (lambda (q) (f-apply q n))))

(define (nfun! name in out) (set! *FLIST* (cons (Function name in out) *FLIST*)))
(define (leave-scope! name in out) (set! *FLIST* (cdr (dropf *FLIST* (lambda (x) (/= x 'scope))))))

; DONE: define `split' for expression of whitespace or ([{}]). splif-at recursive/fold.
;   [] is unscoped expr, {} is scoped expr.

(define (f-apply f lst) (Literal
  (b-app (bytes-append* (map show lst)) (bytes 1) (integer->integer-bytes (Function-ind f)))
  (Function-out f)))

; string->prog :: [String] -> (x :: [String | x])
(define (ch-list->prog lst) (foldr (lambda (x n) (cond
  [(char-whitespace? x) (if (empty? (car n)) n (cons '() n))]
  [(member x '(#\) #\} #\] #\, #\;)) (cons (string x) (if (empty? (car n)) (cdr n) n))]
  [(member x '(#\( #\{ #\[)) ;(begin (displayln
    ;(splitf-at2 n (uc /= (case x [(#\() ")"] [(#\[) "]"] [(#\{) "}"]))))
    ((match-lambda [(list y z) (cons (cons `(,x) y) (cdr z))])
      (splitf-at2 n (uc /= (case x [(#\() ")"] [(#\[) "]"] [(#\{) "}"]))))]
  [else (cons-lit x n)])) '(()) lst))

(define (ch-list->string lst) (map (lambda (x) 
  (if (member (car x) '((#\{) (#\[) (#\())) (ch-list->string x)
      (if (symbol? x) x (list->string x)))) lst))

; tokenize :: (x :: [String | x]) -> (x :: [Literal])
;   reads each string (converted from char-list) and assigns some type.
;   special types include expr (#\[), scope-expr (#\{), and symbol, which is then checked as a
;     function in `parse'.
(define (tokenize lst) (map (lambda (h) (cond
  [(list? h) (Literal (tokenize (cdr h)) (case (car h) [("(") 'expr] [("[") 'ns-expr] [("}") 's-expr]))]
  [(string->number h) (let ([a (string->number h)]) (if (integer? h) (Literal h 'int32)
                                                                     (Literal h 'float64)))]
  [else (Literal h 'symbol)])) lst))

(define (parse lst) (match lst
  [(cons (Literal a 'expr) r) (parse (cons (parse a) r))]
  [(list-rest a (Literal sd 'symbol) b) (f-find-and-apply sd a (parse b))]
  [(cons (Literal m 'symbol) b) (f-find-and-apply m (parse b))]
  [(cons a (list)) a]))

(define test1 (ch-list->prog (string->list "ab bc cd")))
(define test0 (Literal (list (Literal 1 'int32)) 'generator))
(define test2 (ch-list->prog (string->list "ab (cd ef) gh")))
(define test3 (tokenize (ch-list->string test2)))

; parsing rules:
;   take until a non-literal or symbol is found.
;     single literal    -> that literal;
;     multiple literals -> generator of those literals
