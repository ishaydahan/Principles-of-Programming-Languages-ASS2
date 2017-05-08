#lang racket
(require "parser.rkt")
(provide (all-defined-out))

; Purpose: Produce a list of lists of length window each with the elements slided by factor window 
; Signature: sliding-window(lst,window)
; Type: [List * Number -> List]
(define sliding-window
  (lambda (lst window)
    (if (< (length lst) window)
        '()
        (cons (take lst window) (sliding-window (cdr lst) window)))))

; Purpose: Compute the greatest node data value in the tree
; Signature: greatest-node(tree)
; Type: [(List(Number) union Number) -> Number]
(define greatest-node
  (lambda (tree)
    (if (not(list? tree)) tree
        (if (empty? tree) -1
            (max (first tree) (greatest-node (second tree)) (greatest-node (third tree)))))))

; Purpose: Compute the number of nodes whose data value is equal to x
; Signature: count-node(tree,x)
; Type: [(List union T1) * T2 -> Number]
(define count-node
  (lambda (tree x)
    (if (not(list? tree)) (if (eq? tree x) 1 0)
        (if (empty? tree) 0
            (+ (count-node (first tree) x) (count-node (second tree) x) (count-node (third tree) x))
            ))))
; Purpose: Compute the mirrored tree
; Signature: mirror-tree(tree)
; Type: [(List union T) -> List]
(define mirror-tree
  (lambda (tree)
    (if (not(list? tree)) tree
        (if (empty? tree) '()
            (list (first tree) (mirror-tree (third tree)) (mirror-tree (second tree)))))))

; Purpose: unparse a Scheme AST into Javascript syntax without considering infix notation
; Signature: unparse->js(ast,output-port)
; Type: [Exp * Output-Port -> Void]
(define unparse->js
  (lambda (ast output-port)
    (letrec (
             (start (lambda () (fprintf output-port "~a" (unparse->js* ast))))
             (unparse->js* (lambda (ast)
                             (cond ((def-exp? ast) (format "const ~a = ~a;"
                                                           (unparse->js* (def-exp->var ast))
                                                           (unparse->js* (def-exp->val ast))))
                                   ((cexp? ast)
                                    (cond ((num-exp? ast)  (num-exp->val ast))
                                          ((bool-exp? ast) (if (bool-exp->val ast)
                                                               "true"
                                                               "false"))
                                          ((str-exp? ast)  (~s(str-exp->val ast)))
                                          ((var-exp? ast)  (if (eq? '= (var-exp->var ast)) "=="  (var-exp->var ast)))

                                          ((proc-exp? ast) (format "(~a) => { ~a }"
                                                                   (string-join (map ~a(map unparse->js* (proc-exp->params ast))) ",")
                                                                   (string-join (map ~a(map unparse->js* (proc-exp->body ast))) "; ")))
                                          ((if-exp? ast)   (format "~a ? ~a : ~a"
                                                                   (unparse->js* (if-exp->test ast))
                                                                   (unparse->js* (if-exp->then ast))
                                                                   (unparse->js* (if-exp->else ast))))
                                          ((let-exp? ast)  (format "let ~a; ~a;"
                                                                   (string-join (map ~a(map (lambda (b) (format "~a = ~a"
                                                                                                                (unparse->js* (binding->var b))
                                                                                                                (unparse->js* (binding->val b))))
                                                                                            (let-exp->bindings ast)))", ")
                                                                   (string-join (map ~a(map unparse->js* (let-exp->body ast))) "; ")))
                                          ((app-exp? ast)  (format "~a(~a)"
                                                                   (unparse->js* (app-exp->rator ast))
                                                                   (string-join (map ~a(map unparse->js* (app-exp->rands ast))) ",")))
                                          (else (error "Unknown exp type: " ast))))
                                   (else (error "Unknown exp type: " ast))))))
      (start))))


; Purpose: unparse a Scheme AST into Javascript syntax while considering infix notation
; Signature: unparse->js-infix(ast,output-port)
; Type: [Exp * Output-Port -> Void]
(define unparse->js-infix
  (lambda (ast output-port)
    (letrec (
             (start (lambda () (fprintf output-port "~a" (unparse->js-infix* ast))))
             (unparse->js-infix* (lambda (ast)
                                   (cond ((def-exp? ast) (format "const ~a = ~a;"
                                                                 (unparse->js-infix* (def-exp->var ast))
                                                                 (unparse->js-infix* (def-exp->val ast))))
                                         ((cexp? ast)
                                          (cond ((num-exp? ast)  (num-exp->val ast))
                                                ((bool-exp? ast) (if (bool-exp->val ast)
                                                                     "true"
                                                                     "false"))
                                                ((str-exp? ast)  (~s(str-exp->val ast)))
                                                ((var-exp? ast) (cond((eq? '= (var-exp->var ast)) " == ")
                                                                     ((eq? '* (var-exp->var ast)) " * ")
                                                                     ((eq? '+ (var-exp->var ast)) " + ")
                                                                     ((eq? '/ (var-exp->var ast)) " / ")
                                                                     ((eq? '- (var-exp->var ast)) " - ")
                                                                     (else (var-exp->var ast))))
                                          
                                                ((proc-exp? ast) (format "(~a) => { ~a }"
                                                                         (string-join (map ~a(map unparse->js-infix* (proc-exp->params ast))) ",")
                                                                         (string-join (map ~a(map unparse->js-infix* (proc-exp->body ast))) "; ")
                                                                         ))
                                                ((if-exp? ast) (format "~a ? ~a : ~a"
                                                                       (unparse->js-infix* (if-exp->test ast))
                                                                       (unparse->js-infix* (if-exp->then ast))
                                                                       (unparse->js-infix* (if-exp->else ast))))
                                                ((let-exp? ast) (format "let ~a; ~a;"
                                                                        (string-join (map ~a(map (lambda (b) (format "~a = ~a"
                                                                                                                     (unparse->js-infix* (binding->var b))
                                                                                                                     (unparse->js-infix* (binding->val b))))
                                                                                                 (let-exp->bindings ast)))", ")
                                                                        (string-join (map ~a(map unparse->js-infix* (let-exp->body ast))) "; ")))
                                                ((app-exp? ast) (if (string? (unparse->js-infix* (app-exp->rator ast)))
                                                                    (format "(~a)"
                                                                            (string-join (map ~a(map unparse->js-infix* (app-exp->rands ast))) (~a(unparse->js-infix* (app-exp->rator ast)))))
                                                                    (format "~a(~a)"
                                                                            (unparse->js-infix* (app-exp->rator ast))
                                                                            (string-join (map ~a(map unparse->js-infix* (app-exp->rands ast))) ","))))
                                                (else (error "Unknown exp type: " ast))))
                                         (else (error "Unknown exp type: " ast))))))
      (start))))