;; Each base64 character is one brainfuck instruction with repeat.
;;
;; 3 low bits: opcode (from headsecks)
;;
;;     0 1 2 3 4 5 6 7
;;     + - < > . , [ ]
;;
;; 3 high bits: how many times to repeat (0 = once only, don't repeat)

(define base64-alphabet
  (string-append "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                 "abcdefghijklmnopqrstuvwxyz"
                 "0123456789"
                 "+/"))

(define lookup "+-<>.,[]")

(define (string-index s char)
  (let loop ((i 0))
    (cond ((= i (string-length s)) #f)
          ((char=? char (string-ref s i)) i)
          (else (loop (+ i 1))))))

(define (brainfuck->base64 source-code)
  (define (insn->opcode insn)
    (or (string-index lookup insn) (error "Huh?")))
  (define (rollup stride base64)
    (let ((opcode (insn->opcode (car stride)))
          (repeat (- (length stride) 1)))
      (string-append
       base64 (string
               (string-ref base64-alphabet
                           (bitwise-ior
                            opcode (arithmetic-shift repeat 3)))))))
  (let loop ((i 0) (string "") (stride '()))
    (if (= i (string-length source-code))
        (rollup stride string)
        (let ((insn (string-ref source-code i)))
          (if (and (not (null? stride))
                   (or (not (eqv? insn (car stride)))
                       (= 8 (length stride))))
              (loop (+ i 1) (rollup stride string) (list insn))
              (loop (+ i 1) string (cons insn stride)))))))

(define (read-base64->brainfuck)
  (define (decode-opcode byte)
    (let ((opcode (bitwise-and #b111 byte))
          (repeat (bitwise-and #b111 (arithmetic-shift byte -3))))
      (make-string (+ 1 repeat) (string-ref lookup opcode))))
  (define (base64-char->integer char)
    (or (string-index base64-alphabet char)
        (error "Not base")))
  (let loop ((source-code ""))
    (let ((char (read-char)))
      (if (eof-object? char) source-code
          (loop (string-append source-code
                               (decode-opcode
                                (base64-char->integer char))))))))

(define (base64->brainfuck string)
  (call-with-port (open-input-string string)
                  (lambda (in)
                    (parameterize ((current-input-port in))
                      (read-base64->brainfuck)))))

(define hello
  (string-append
   "++++++++[>++++[>++>+++>+++>+<<<<-]>+>->+>>+[<]<-]>>.>>---.++++++"
   "+..+++.>.<<-.>.+++.------.--------.>+.>++."))

(define (displayln x) (display x) (newline))
(displayln (brainfuck->base64 hello))
(displayln (string-length (brainfuck->base64 hello)))
(displayln (equal? hello (base64->brainfuck (brainfuck->base64 hello))))
