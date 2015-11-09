#lang plai

(define (interp expr env)
    (type-case FCFAEL expr
        [num (n) (numV n)]
        [id (n) (lookup n env)]
        [bool (b) (boolV b)]
        [listC (l) (listC l)]
        [ifC (c t e) (if (evalBool (interp c env))
                        (interp t env)
                        (interp e env))]
        [isequal? (x y) (evalEqual (interp x env) (interp y env))]
        [op (f l) (opV f (interp l env))]
        [binop (f l r) (opV f (interp l env) (interp r env))]
        [boolOpBin (f l r) (boolop f (interp l env) (interp r env))]

        [fun (params f) (closureV params f env)]
        [app (fun-expr arg-expr) 
             (local ([define fun-val (interp fun-expr env)])
               (if (checkAll arg-expr env)
                   (interp (closureV-body fun-val)
                           (aux (closureV-param fun-val) arg-expr (closureV-env fun-val)))
                   (error "Un simbolo no esta en el ambiente")))]))
;;------------------------------------------
(define (desugar expr)
    (type-case RCFAELS expr
        [idS (x) (id x)]
        [numS (n) (num n)]
        [boolS (b) (bool b)]
        [listS (l) (listS l)]
        [ifS (c t e) (if (desugar c) (desugar t) (desugar e))]
        [equalS?  (x y) (equalS? (desugar x) (desugar y))]
        [opS (f o) (op f (desugar o))]
        [binopS (f l r) (binopS f (desugar l) (desugar r))]
        [ boolOpBinS (f l r) (  boolOpBinS f (desugar l) (desugar r))]
        [funS (params b) (fun params (desugar b))]
        [appS (fun args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]
      
        [withS (bindings b) (app (fun (map (lambda (bind) 
                                        (bind-name bind)) bindings)
                                        (desugar b))
                                    (map (lambda (bind)
                                        (desugar (bind-val bind))) bindings))]
        [with*S (bindings b) (matryoshka bindings b)]))
