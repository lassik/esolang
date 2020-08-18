;; Each byte:
;;
;; 3 low bits: opcode (from headsecks)
;;     0 1 2 3 4 5 6 7
;;     + - < > . , [ ]
;; 5 high bits: how many times to repeat (0 = once only, don't repeat)

(define lookup "+-<>.,[]")

(define (brainfuck->rle source-code)
  (define (insn->opcode insn)
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
                       (= 32 (length stride))))
              (loop (+ i 1) (rollup stride bytes) (list insn))
              (loop (+ i 1) bytes (cons insn stride)))))))

(define (read-rle-brainfuck)
  (define (decode-byte byte)
    (let ((opcode (bitwise-and #b111 byte))
          (repeat (bitwise-and #b11111 (arithmetic-shift byte -3))))
      (make-string (+ 1 repeat) (string-ref lookup opcode))))
  (let loop ((source-code ""))
    (let ((byte (read-u8)))
      (if (eof-object? byte)source-code
          (loop (string-append source-code (decode-byte byte)))))))

(define (dump output-file source-code)
  (call-with-port (open-binary-output-file output-file)
                  (lambda (out)
                    (write-bytevector (brainfuck->rle source-code) out))))

(define (write-hello)
  (dump "hello.bin"
        (string-append
         "++++++++[>++++[>++>+++>+++>+<<<<-]>+>->+>>+[<]<-]>>.>>---.++++++"
         "+..+++.>.<<-.>.+++.------.--------.>+.>++.")))

(define (read-hello)
  (call-with-port (open-binary-input-file "hello.bin")
                  (lambda (in)
                    (parameterize ((current-input-port in))
                      (read-rle-brainfuck)))))

(display (read-hello))
(newline)
