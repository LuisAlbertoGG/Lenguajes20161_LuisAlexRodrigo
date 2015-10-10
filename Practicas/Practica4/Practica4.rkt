


(require "practica4-base.rkt")


(define (desugar exprecion)
  (type-case FAES exprecion
    [numS (numero) (num numero)]
    [withS (bindings b)
           (app (fun (map (lambda (x)(bind-name x)) bindings)(desugar b))
                (map (lambda (bind)(desugar (bind-val bind))) bindings))]
    [with*S (bindings b)
            (app (fun (map (lambda (x) (bind-name x)) bindings) (desugar b))
                 (map (lambda (x) (desugar (bind-val x))) bindings))]
    [idS (i) (id i)]
    [funS (params p) (fun params (desugar p))]
    [appS (a lst) (app (desugar a) (map desugar lst))]
    [binopS (b l r) (binop b (desugar l) (desugar r))]))


;;Ejercicio 3

(define (opbin op d e)
  (cond
    [(numV (op (numV d) (numV e)))]))
(define (lookup b env)
  (env b))

;;Ejercicio 4

(define (interp expr env)
  (type-case FAE expr
    [num (a) (numV a)]
    [id (b) (lookup b env)]
    [fun (params body) (closureV params body env)]
    [app (fun args) '()] ;Falta
    [binop (c d e) (numV (c (numV (interp d env)) (numV (interp e env))))]
    ))

(define (rinterp expr)
  (interp expr (mtSub)))
