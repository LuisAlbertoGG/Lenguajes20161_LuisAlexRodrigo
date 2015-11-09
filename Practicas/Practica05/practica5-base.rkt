#lang plai

(define-type Binding
  [bind (name symbol?) (val RCFAELS?)])

(define-type RCFAELS
  [numS (n number?)]
  [MEmpty (n number?)]
  [withS (bindings (listof bind?))
         (body RCFAELS?)]
  [with*S (bindings (listof bind?))
          (body RCFAELS?)]
  [idS (name symbol?)]
  [funS (params (listof symbol?))
        (body RCFAELS?)]
  [appS (fun RCFAELS?)
        (args (listof RCFAELS?))]
  [binopS (f procedure?)
         (l RCFAELS?)
         (r RCFAELS?)]
  [boolOpBinS (f symbol?)
             (l RCFAELS?)
             (r RCFAELS?)]
  [opS (f symbol?)
       (rst RCFAELS?)]
  [boolS (b boolean?)]
  [ifS (cond RCFAELS?)
       (case1 RCFAELS?)
       (case2 RCFAELS?)]
  [equalS? (comp1 RCFAELS?)
           (comp2 RCFAELS?)]
  [listS (n RCFAELS?)
         (rst RCFAELS?)])

(define-type FCFAEL
  [num (n number?)]
  [id (name symbol?)]
  [fun (params (listof symbol?))
       (body FCFAEL?)]
  [app (fun FCFAEL?)
       (args (listof FCFAEL?))]
  [binop (f procedure?)
         (l FCFAEL?)
         (r FCFAEL?)]
  [boolOpBin (f symbol?)
             (l FCFAEL?)
             (r FCFAEL?)]
  [op (f symbol?)
      (rst FCFAEL?)]
  [bool (b boolean?)]
  [isequal? (cond1 FCFAEL?)
            (cond2 FCFAEL?)]
  [ifC (cond FCFAEL?)
      (case1 FCFAEL?)
      (case2 FCFAEL?)]
  [listC (n FCFAEL?)
         (rst FCFAEL?)])

(define-type FCFAEL-Value
  [numV (n number?)]
  [MEmptyV]
  [closureV (param (listof symbol?))
            (body FCFAEL?)
            (env Env?)]
  [boolV (b boolean?)]
  [listV (n FCFAEL-Value?)
         (rst FCFAEL-Value?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (value FCFAEL-Value?) 
        (env Env?)])


(define (parse-bindings lst allow)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (or (boolean? bindRep) allow)
        (map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))

(define (elige s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    ;nuevos
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(and) 'and]
    [(or) 'or]
    ['and 'and]
    ['or 'or]
    [(inc) 'inc]
    [(dec) 'dec]
    [(zero?) 'zero?]
    [(num?) 'num?]
    [(neg) 'neg]
    [(bool?) 'bool?]
    [(first) 'first]
    [(rest) 'rest]
    [(empty?) 'empty?]
    [(list?) 'list?]))
    

(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))

(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))

(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(if) (IfS (parse (cadr sexp)) (parse(caddr sexp))(parse(cadddr sexp)))]
       [(list) (if(empty? (cdr sexp))
                   (ConsS (MEmptyS) (MEmptyS))
                   (parseL (cdr sexp)))]
       [(rec) (recS (caadr sexp) (parse (cadadr sexp)) (parse (caddr sexp)))]
       [(equal?) (Equal?S (parse (cadr sexp)) (parse (caddr sexp)))]
       [(with) (withS (parse-bindings (cadr sexp) #f) (parse (caddr sexp)))]
       [(with*) (with*S (parse-bindings (cadr sexp) #t) (parse (caddr sexp)))]
       [(fun) (funS (cadr sexp) (parse (caddr sexp)))]
       [(+ - / * > < <= >= and or) (binopS (elige-binop (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [(inc dec zero? num? neg bool? first rest empty? list?) (opS (elige-op (car sexp)) (parse (cadr sexp)))]
       [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))
