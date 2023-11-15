#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; ----------------- STRUCTS ------------------------------
(struct object (class-name fields))
(struct class (super-name field-names method-env))
(struct method (vars body super-name fields))

; Define o ambiente de classes como um ambiente inicialmente vazio
(define class-env '())  ; É uma lista de class, onde é armazenada o nome da classe e a struct class referente a ela.

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (deref (apply-env Δ v))]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args)
      (let* ([args-with-value (map-value-of args Δ)]
          [obj (value-of e Δ)])
      (apply-method (find-method (object-class-name obj) mth) obj args-with-value))
    ]
    [(ast:super (ast:var c) args) 
      (let ([args-with-value (map-value-of args Δ )]
          [obj (apply-env Δ "self")])
      (apply-method (find-method (apply-env Δ "super") (ast:var-name args)) obj args-with-value ))
    ]
    [(ast:self) (apply-env Δ "self")]

    
    [(ast:new (ast:var c) args)
      (let* ([args-with-value (map-value-of args Δ)]
          [obj 
            (let* ([class (find-class c)]
                [field-names (class-field-names class)]
                [fields (map (λ (field-name) (newref null)) field-names)])
            (object c fields))
          ])
      ; (apply-method (find-method c "initialize") obj args-with-value)
       (display (if (zero? 0) (find-method c "initialize") "TESTE"))
      obj)
    ]
    [e (raise-user-error "unimplemented-value-of-construction: " e)]))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (begin (setref! (apply-env Δ x) (value-of e Δ)) 42)]
    [(ast:print e) 
      (display (value-of e Δ))
      (newline)]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (for ([s stmts]) (result-of s Δ))]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) (result-of s1 Δ) (result-of s2 Δ))]
    [(ast:while e s) (if (value-of e Δ)
                         (begin
                           (result-of s Δ)
                           (result-of stmt Δ))
                         'done)]
    [(ast:local-decl (ast:var x) s) (result-of s (extend-env x (newref 'null) Δ))]
    [(ast:send e (ast:var mth) args)
      (let* ([args-with-value (map-value-of args Δ)]
          [obj (value-of e Δ)])
      (apply-method (find-method (object-class-name obj) mth) obj args-with-value))
    ]
    [(ast:super (ast:var c) args)
      (let ([args-with-value (map-value-of args Δ )]
          [obj (apply-env Δ "self")])
      (apply-method (find-method (apply-env Δ "super") c) obj args-with-value ))
    ]
    [e (raise-user-error "unimplemented-result-of-construction: " e)]))

(define (add-class class-name class-list)
  (if (class-exists? class-name class-list)
      (raise-user-error "Já existe uma classe com a mesma definição: " class-name)
      (set! class-env (cons (cons class-name class-list) class-env))))

(define (class-exists? class-name class-list)
  (let ([existing-class (find-class-exists class-name)])
    (and existing-class
         (equal? (class-field-names existing-class) (class-field-names class-list))
         (equal? (class-method-env existing-class) (class-method-env class-list)))))

(define (find-class-exists class-name)
  (let ([maybe-pair (assoc class-name class-env)])
    (if (pair? maybe-pair)
        (cdr maybe-pair)
        #f)))

(define (find-class class-name)  ; Busca por uma classe no class-env. Dá pra refazer essa função
  (let ([class-pair (assoc class-name class-env)]) ; esse assoc busca o class-name no class-env e guarda no class-pair
    (if class-pair ; se a classe existe
        (cdr class-pair) ; retorna a classe e não o nome
        (raise-user-error "Classe não encontrada: " class-name)))) ; lança um erro

(define (get-field-names fields) ; pega os nomes dos fields
  (map (lambda (field)    ; faz o map na fields. Se field for string retorna, caso field seja uma estrutura, então pega o nome dela
         (if (string? field)
             field
             (ast:var-name field)))
       fields))

(define (append-field-names super-fields self-fields) ; faz a união das duas listas mas trata quando algum field já existia no pai
  (foldr (lambda (field acc)
           (if (member field acc) ; se já está na super-fields
               (append acc (list (string-append field "%1"))) ; então coloca um %1 no fim do nome para não dar conflito
               (append acc (list field)))) ; caso contrário, só adiciona no acumulador
         self-fields
         super-fields))

(define (merge-method m-decls super-name fields)
  (append
   (map (lambda (m-decl) (create-method super-name fields m-decl)) m-decls)
   (class-method-env (find-class super-name))))

(define (create-method super-name fields m-decl)
  (list
   (ast:var-name (ast:method-name m-decl))
   (method (map ast:var-name (ast:method-params m-decl)) (ast:method-body m-decl) super-name fields)))

(define (apply-method method self args)
  (let* ([args-with-refs (map newref args)]
         [extended-env (extend-env "self" self
                                   (extend-env "super" (method-super-name method)
                                               empty-env))]
         [method-env (bind-vars (method-fields method)
                                       (object-fields self)
                                       extended-env)])
    (result-of (method-body method)
               (bind-vars (method-vars method)
                                 args-with-refs
                                 method-env))))

(define (bind-vars vars values env)
  (for ([var vars] [val values])
    (set! env (extend-env var val env)))
  env)

(define (find-method class-name method-name)
  (let ([m-env (class-method-env (find-class class-name))])
    (let ([maybe-pair (assoc method-name m-env)])
      (if (pair? maybe-pair)
          (cadr maybe-pair)
          (raise-user-error "Método não encontrado: " method-name)))))

(define (map-value-of exps Δ)
  (map (λ (exp) (value-of exp Δ)) exps))

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
        (add-class "object" (class #f '() '()))  ; Adicona a classe object a lista de classes global. Super é #f, field-names method-env são vazios
        (for ([decl decls])  ; Itera sobre a lista de declarações. Pra cada decl em decls
          (let* ([class-name (ast:var-name (ast:decl-name decl))] ; pega o nome da classe que fica no decl
                [super-name (ast:var-name (ast:decl-super decl))] ; pega o nome da super-classe que fica no decl
                [super-fields (class-field-names (find-class super-name))] ; pega os nomes dos fields da classe pai a partir da classe
                ;pega os filds da classe atual; pega os nomes; gera uma lista com todos os fields
                [fields (append-field-names super-fields (get-field-names (ast:decl-fields decl)))]
                ; pega os metodos do decl; chama o merge-method passando os metodos, o super-name e os fields; 
                [methods (merge-method (ast:decl-methods decl) super-name fields)]
                [class (class super-name fields methods)])
            (add-class class-name class)
            ))
        (result-of stmt init-env)
        )])) ; Avalia o stmt, é quando não há classes no programa. 