


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
