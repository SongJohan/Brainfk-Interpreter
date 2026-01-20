#lang racket
(define-struct head (left right val) #:transparent)
(define newtape (head empty empty 0))
(define (show b x) (if b (display x) (display (integer->char x))))
(define (out exp stack) (match (first exp)
                          ['\[ (out (rest exp) (add1 stack))]
                          ['\] (if (= 0 stack) (rest exp) (out (rest exp) (sub1 stack)))]
                          [x (out (rest exp) stack)]
                          ))
(define (in exp stack acc) (match (first exp)
                             ['\[ (in (rest exp) (add1 stack) (cons '\[ acc))]
                             ['\] (if (= 0 stack) (reverse acc) (in (rest exp) (sub1 stack) (cons '\] acc)))]
                             [x (in (rest exp) stack (cons x acc))]))

(define (interp exp tape)
  (match exp
    [`(+ ,x ...) (interp x (make-head (head-left tape) (head-right tape) (add1 (head-val tape))))]
    [`(- ,x ...) (interp x (make-head (head-left tape) (head-right tape) (sub1 (head-val tape))))]
    [`(\. ,x ...) (display (head-val tape)) (interp x tape)]
    [`(c ,x ...) (display (integer->char (head-val tape))) (interp x tape)]
    [`(\, ,x ...) (interp x (make-head (head-left tape) (head-right tape) (read)))]
    [`(> ,x ...) 
     (if (empty? (head-right tape))
         (interp x (make-head (cons (head-val tape) (head-left tape)) empty 0))
         (interp x (make-head (cons (head-val tape) (head-left tape)) (rest (head-right tape)) (first (head-right tape)))))]
    [`(< ,x ...)
     (if (empty? (head-left tape))
         (interp x (make-head empty (cons (head-val tape) (head-right tape)) 0))
         (interp x (make-head (rest (head-left tape)) (cons (head-val tape) (head-right tape))  (first (head-left tape)))))]
    [`(\[ ,x ...) (if (= 0 (head-val tape))
                                (interp (out x 0) tape)
                                (interp (out x 0) (interp-loop (in x 0 empty) tape)))]
    [`(,x ,y ...) (interp y (make-head (head-left tape) (head-right tape) x))]
    [_ tape]))

(define (interp-loop exp tape)
  (if (= 0 (head-val tape))
      tape
      (interp-loop exp (interp exp tape))))

(define (viewtape tape) (append (reverse (head-left tape)) (list '- (head-val tape) '-) (head-right tape)))

(define (compile bfk) (filter (λ (a) (or (symbol=? a '+) (symbol=? a '-) (symbol=? a '\,) (symbol=? a '\.) (symbol=? a '>) (symbol=? a '<) (symbol=? a '\[) (symbol=? a '\]) (symbol=? a 'c)))
                       (map (λ (x) (string->symbol (string x))) (string->list bfk))))

(define (run exp) (viewtape (interp (compile exp) newtape)))

;; Sample programs
(define helloWorld "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>c>---c+++++++cc+++c>>c<-c<c+++c------c--------c>>+c>++c")
(define bubbleSort ">>,[>>,]<<[[<<]>>>>[<<[>+<<+>-]>>[>+<<<<[->]>[<]>>-]<<<[[-]>>[>+<-]>>[<<<+>>>-]]>>[[<+>-]>>]<]<<[>>+<<-]<<]>>>>[.>>]")
(define rev ">,[>,]<[.<]")