#lang racket
(require "parser.rkt")
(require "rewrite.rkt")
(require "hw2.rkt")

(define passed #t)

(define test-parse (lambda (test-num in out f g)
                     (letrec(
                             (op (open-output-string))
                             (close (lambda (func) (close-output-port op) func )))
                 
                       (f (g in) op)
                       (if(not(string=? 
                               (get-output-string op)
                               out))
                          (close
                           (begin (display (string-append "FAIL TEST " test-num " EXPECTED:" ))(newline)(newline)
                                  (display out)(newline)(newline)
                                  (display "GOT:")(newline)(newline)
                                  (display (get-output-string op))(newline)(newline)
                                  (set! passed #f)))
                          (void)))))

(define test (lambda (test-num in out)     
               (if(not(equal? 
                       in
                       out))
                  (begin (display (string-append "FAIL TEST " test-num " EXPECTED:" ))(newline)(newline)
                         (display out)(newline)(newline)
                         (display "GOT:")(newline)(newline)
                         (display in)(newline)(newline)
                         (set! passed #f))
                  (void))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sliding-window
(test "sliding-window - 1" (sliding-window '(1 2 3 4 5) 1) '((1) (2) (3) (4) (5)))
(test "sliding-window - 2" (sliding-window '(1 2 3 4 5) 2) '((1 2) (2 3) (3 4) (4 5)))
(test "sliding-window - 3" (sliding-window '(1 2 3 4 5) 3) '((1 2 3) (2 3 4) (3 4 5)))
(test "sliding-window - 4" (sliding-window '(1 2 3 4 5) 5) '((1 2 3 4 5)))
(test "sliding-window - 5" (sliding-window '(1 2 3 4 5) 4) '((1 2 3 4) (2 3 4 5)))
(test "sliding-window - 5" (sliding-window '(1) 1) '((1)))

;greatest-node
(test "greatest-node - 1" (greatest-node '(1 (7 (24 15 ()) (12 25 12)) (12 () ()))) 25)
(test "greatest-node - 2" (greatest-node 5) 5)
(test "greatest-node - 3" (greatest-node '()) -1)
(test "greatest-node - 4" (greatest-node '(0 (0 (0 () ()) ()) ())) 0)

;count-node
(test "count-node - 1" (count-node '(1 (#f () 1) hi) 'hi) 1)
(test "count-node - 2" (count-node '(1 (#f () 1) hi) 1) 2)
(test "count-node - 3" (count-node '() 20) 0)
(test "count-node - 4" (count-node 5 5) 1)

;mirror-tree
(test "mirror-tree - 1" (mirror-tree '(5 (#t 4 2) (12 () 3))) '(5 (12 3 ()) (#t 2 4)))
(test "mirror-tree - 2" (mirror-tree '()) '())
(test "mirror-tree - 3" (mirror-tree "s") "s")
(test "mirror-tree - 4" (mirror-tree '(1 (2 (3 4 5) 6) 7)) '(1 7 (2 6 (3 5 4))))
(test "mirror-tree - 5" (mirror-tree '(1 (2 (3 () ()) ()) ())) '(1 () (2 () (3 () ()))))

;rewrite-all-let*
(test "rewrite-all-let* - 1" (rewrite-all-let* (parse '(let* () 7)))
      '(let-exp () ((num-exp 7))))

(test "rewrite-all-let* - 2" (rewrite-all-let* (parse '(let* ((x 5)) 7)))
      '(let-exp ((binding (var-exp x) (num-exp 5))) ((num-exp 7))))

(test "rewrite-all-let* - 3" (rewrite-all-let* (parse '(let* ((x 5) (y (f x))) (+ x y) (- y x))))
      '(let-exp
        ((binding (var-exp x) (num-exp 5)))
        ((let-exp
          ((binding (var-exp y) (app-exp (var-exp f) ((var-exp x)))))
          ((app-exp (var-exp +) ((var-exp x) (var-exp y)))
           (app-exp (var-exp -) ((var-exp y) (var-exp x))))))))

(test "rewrite-all-let* - 4" (rewrite-all-let* (parse '(let* ((x 5) (y 3)) (let* ((z 4) (w 3)) w) (- y x))))
      '(let-exp ((binding (var-exp x) (num-exp 5)))
                ((let-exp ((binding (var-exp y) (num-exp 3)))
                          ((let-exp ((binding (var-exp z) (num-exp 4)))
                                    ((let-exp ((binding (var-exp w) (num-exp 3)))
                                              ((var-exp w)))))
                           (app-exp (var-exp -) ((var-exp y) (var-exp x))))))))

(test "rewrite-all-let* - 5" (rewrite-all-let* (parse '(let* ((x (let* ((e 2) (f 5)) 4))) 6)))
      '(let-exp
        ((binding
          (var-exp x)
          (let-exp ((binding (var-exp e) (num-exp 2))) ((let-exp ((binding (var-exp f) (num-exp 5))) ((num-exp 4))))))) ((num-exp 6))))

(test "rewrite-all-let* - 6" (rewrite-all-let* (parse '(+ 4 (let* ((x 4) (y x)) (+ y x) (display 7 9 8)))))
      '(app-exp (var-exp +) ((num-exp 4)
                             (let-exp ((binding (var-exp x) (num-exp 4)))
                                      ((let-exp ((binding (var-exp y) (var-exp x)))
                                                ((app-exp (var-exp +) ((var-exp y) (var-exp x)))
                                                 (app-exp (var-exp display) ((num-exp 7) (num-exp 9) (num-exp 8))))))))))

(test "rewrite-all-let* - 7" (rewrite-all-let* (parse '(if (let* ((x 4) (y x)) (+ y x) #t) (let* ((x 4) (y x)) (+ y x) (display 7 9 8)) (let* ((x 4) (y x)) (+ y x) (display 7 9 8)))))
      '(if-exp
        (let-exp ((binding (var-exp x) (num-exp 4)))
                 ((let-exp ((binding (var-exp y) (var-exp x)))
                           ((app-exp (var-exp +) ((var-exp y) (var-exp x))) (bool-exp #t)))))
        (let-exp ((binding (var-exp x) (num-exp 4)))
                 ((let-exp ((binding (var-exp y) (var-exp x)))
                           ((app-exp (var-exp +) ((var-exp y) (var-exp x))) (app-exp (var-exp display) ((num-exp 7) (num-exp 9) (num-exp 8)))))))
        (let-exp ((binding (var-exp x) (num-exp 4)))
                 ((let-exp ((binding (var-exp y) (var-exp x)))
                           ((app-exp (var-exp +) ((var-exp y) (var-exp x))) (app-exp (var-exp display) ((num-exp 7) (num-exp 9) (num-exp 8)))))))))

(test "rewrite-all-let* - 8" (rewrite-all-let* (parse '(define x (let* ((x 4) (w 3)) 7))))
      '(def-exp (var-exp x)
         (let-exp ((binding (var-exp x) (num-exp 4)))
                  ((let-exp ((binding (var-exp w) (num-exp 3)))
                            ((num-exp 7)))))))

(test "rewrite-all-let* - 9" (rewrite-all-let* (parse '(lambda (x) (let* ((x 4) (w 3)) 7))))
      '(proc-exp ((var-exp x))
                 ((let-exp ((binding (var-exp x) (num-exp 4)))
                           ((let-exp ((binding (var-exp w) (num-exp 3)))
                                     ((num-exp 7))))))))

(test "rewrite-all-let* - 10" (rewrite-all-let* (parse '(let ((x (let* ((x 4) (w 3)) 7))) (let* ((x 4) (w 3)) 7))))
      '(let-exp ((binding (var-exp x)
                          (let-exp ((binding (var-exp x) (num-exp 4)))
                                   ((let-exp ((binding (var-exp w) (num-exp 3)))
                                             ((num-exp 7)))))))
                ((let-exp ((binding (var-exp x) (num-exp 4)))
                          ((let-exp ((binding (var-exp w) (num-exp 3)))
                                    ((num-exp 7))))))))
;unparse->js
(test-parse "unparse->js - 1"
            '(define x (f x (* 5 x) (= 3 2) y))
            "const x = f(x,*(5,x),==(3,2),y);"
            unparse->js
            parse)

(test-parse "unparse->js - 2"
            '(lambda (x y z) (display "hi!") (if (eq? x y) z (or #f (eq? x z))))
            "(x,y,z) => { display(\"hi!\"); eq?(x,y) ? z : or(false,eq?(x,z)) }"
            unparse->js
            parse)

(test-parse "unparse->js - 3"
            '(lambda (x) (let ((y (+ (f x) 5)) (z y)) (+ z y) #t))
            "(x) => { let y = +(f(x),5), z = y; +(z,y); true; }"
            unparse->js
            parse)

(test-parse "unparse->js - 4"
            '(lambda (x y z) x y z)
            "(x,y,z) => { x; y; z }"
            unparse->js
            parse)

(test-parse "unparse->js - 5"
            '(f (x (g (y (z x)))))
            "f(x(g(y(z(x)))))"
            unparse->js
            parse)

(test-parse "unparse->js - 6"
            '(= 4 5)
            "==(4,5)"
            unparse->js
            parse)

(test-parse "unparse->js - 7"
            '(if #t #f "sss")
            "true ? false : \"sss\""
            unparse->js
            parse)

(test-parse "unparse->js - 8"
            '(lambda (x y z) "bla" (let ((x 5) (y 7) ) 2) "another-bla" )
            "(x,y,z) => { \"bla\"; let x = 5, y = 7; 2;; \"another-bla\" }"
            unparse->js
            parse)

;unparse->js-infix
(test-parse "unparse->js-infix - 1"
            '(+ 5 3 (* 2 (- x y w) 7) z 4)
            "(5 + 3 + (2 * (x - y - w) * 7) + z + 4)"
            unparse->js-infix
            parse)

(test-parse "unparse->js-infix - 2"
            '(lambda (x y) (if (= x y) (+ y y) (* (- x 0) y (/ x 1 y))))
            "(x,y) => { (x == y) ? (y + y) : ((x - 0) * y * (x / 1 / y)) }"
            unparse->js-infix
            parse)

(test-parse "unparse->js-infix - 3"
            '(lambda (x y) (if (= x y) (display q w e r t y) 0))
            "(x,y) => { (x == y) ? display(q,w,e,r,t,y) : 0 }"
            unparse->js-infix
            parse)

(if passed (display "ALL CLEAR") (void))



 



