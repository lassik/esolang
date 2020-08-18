;; Each byte:
;;
;; 3 low bits: opcode (from headsecks)
;;     0 1 2 3 4 5 6 7
;;     + - < > . , [ ]
;; 5 high bits: how many times to repeat (0 = once only, don't repeat)

(define (brainfuck->rle source-code)
  (define (insn->opcode insn)
    (define lookup "+-<>.,[]")
    (let loop ((i 0))
      (cond ((= i (string-length lookup)) (error "Huh?"))
            ((char=? insn (string-ref lookup i)) i)
            (else (loop (+ i 1))))))
  (define (rollup stride bytes)
    (let ((opcode (insn->opcode (car stride)))
          (repeat (- (length stride) 1)))
      (bytevector-append bytes (bytevector
                                (bitwise-ior
                                 opcode (arithmetic-shift repeat 3))))))
  (let loop ((i 0) (bytes (bytevector)) (stride '()))
    (if (= i (string-length source-code))
        (rollup stride bytes)
        (let ((insn (string-ref source-code i)))
          (if (and (not (null? stride))
                   (or (not (eqv? insn (car stride)))
                       (= 31 (length stride))))
              (loop (+ i 1) (rollup stride bytes) (list insn))
              (loop (+ i 1) bytes (cons insn stride)))))))

(define (dump output-file source-code)
  (call-with-port (open-binary-output-file output-file)
                  (lambda (out)
                    (write-bytevector (brainfuck->rle source-code) out))))

(dump "hello.bin"
      (string-append
       "++++++++[>++++[>++>+++>+++>+<<<<-]>+>->+>>+[<]<-]>>.>>---.++++++"
       "+..+++.>.<<-.>.+++.------.--------.>+.>++."))
