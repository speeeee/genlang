#lang racket
(provide show Literal)

(define test0 (Literal (list (Literal 1 'int32)) 'generator))

; this is a simple implementation of what will be compiled to the Genos bytecode.
; *MUTABLE-VALUE*

(struct Literal (val type))
(struct Function (name in out))

; *FLIST* :: [Function | SCOPE]
;   new functions are cons'ed in.
(define *FLIST* (list 'scope))

; for now: int32, float64, bint32, bfloat64

; show :: Literal -> ByteString
;   encodes in little-endian.
(define (show l)
  (case (Literal-type l)
    [(int32) (integer->integer-bytes (Literal-val l) 4 #f)]
    [(float64) (real->floating-point-bytes (Literal-val l) 8)]
    [(byte) (Literal-val l)]
    [(generator) (bytes-append* (map show 
                   (cons (Literal (bytes 26 #| Generator |#) 'byte)
                     (cons (Literal (integer->integer-bytes (length (Literal-val l)) 4 #f) 'byte)
                         (Literal-val l)))))]
    [else #"Error"]))
