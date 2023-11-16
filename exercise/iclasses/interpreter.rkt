#lang racket

; Dener Luis Basílio Teodoro 201835001
; Lásaro de Almeida Deodoro  201835004

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

(struct object (class_name fields))
(struct class (super_name name_fields methods))
(struct method (vars body super_name fields))

(define program_class '())

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (not (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (deref (apply-env Δ v))] ; esta implementação só funciona para variáveis imutáveis
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (newref (value-of e1 Δ)) Δ))]
    [(ast:send e (ast:var mth) args) 
                (let* ([value_args (apply-value-of args Δ)]
                      [objeto (value-of e Δ)])
                      (apply_method (search_method (object-class_name objeto) mth) objeto value_args)
                )]
    [(ast:super (ast:var c) args) (let ([value_args (apply-value-of args Δ)]
                      [objeto (apply-env Δ "self")])
                      (apply_method (search_method (apply-env Δ "super")  (ast:var-name args)) objeto value_args)
                )]
    [(ast:self) (apply-env Δ "self")]
    [(ast:new (ast:var c) args) 
      (let* ([value_args (apply-value-of args Δ)]
              [objeto (new_object c value_args)]
            )
            (apply_method (search_method c "initialize") objeto value_args)
            objeto
      )]
    [e (raise-user-error "unimplemented-construction: " e)]))

(define (apply_method method self args)
  (let* ([args-refs (map newref args)] ; Coloca os valores em memória e obtém referências para eles.
         [class_env (extend-env "self" self (extend-env "super" (method-super_name method) empty-env))]
         [method_env (bind-vars (method-fields method) (object-fields self) class_env)]; nome campos com os endereços de memória
         [method_env_vars (bind-vars (method-vars method) args-refs method_env)]
          )
    (result-of (method-body method) method_env_vars))
)

(define (bind-vars vars values env)
  (for ([var vars] [val values])
    (set! env (extend-env var val env))
  )
  env
)

(define (apply-value-of exps Δ)
  (map (lambda (exp) (value-of exp Δ)) exps))

(define (new_object class_name value_args)
    (let* ([name_fields (class-name_fields (search_class class_name))]
           [fields (map (lambda (f_name) (newref null)) name_fields)]
          )
          (object class_name fields)
    )
)

(define (search_method class_name method_name)
    (let ([methods (class-methods (search_class class_name))])
      (let ([found-method (assoc method_name methods)])
        (if found-method
            (cadr found-method)
            (raise-user-error "[ERROR] - Método não encontrado: " method_name)
        )
      )
    )
)

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (begin 
                                  (setref! (apply-env Δ x) (value-of e Δ))
                                  42)]
    [(ast:print e) (display (value-of e Δ)) 
                          (newline)]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (for ([l stmts])
                             (result-of l Δ)
                            )]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) 
                                (result-of s1 Δ)
                                (result-of s2 Δ))]
    [(ast:while e s) (if (value-of e Δ)
                          (begin
                            (result-of s Δ)
                            (result-of stmt Δ)
                          )
                          #f
                      )]
    [(ast:local-decl (ast:var x) s)  (result-of s (extend-env x (newref 'empty) Δ))]
    [(ast:send e (ast:var mth) args) 
                (let* ([value_args (apply-value-of args Δ)]
                      [objeto (value-of e Δ)])
                    (apply_method (search_method (object-class_name objeto) mth) objeto value_args)
                )]
    [(ast:super (ast:var c) args) (let ([value_args (apply-value-of args Δ)]
                      [objeto (apply-env Δ "self")])
                      (apply_method (search_method (apply-env Δ "super") c) objeto value_args)
                )]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

(define (append_class class-name classe)
  (if (already_exists_class? class-name classe)
      (raise-user-error "[ERROR] - A Classe já existe - " class-name)
      (set! program_class (cons (cons class-name classe) program_class))
  )
)

(define (already_exists_class? class_name classe)
  (let ([old_class (search_class_in_program class_name)])
    (and old_class 
      (equal? (class-methods old_class) (class-methods classe))
      (equal? (class-name_fields old_class) (class-name_fields classe))
    )
  )
)

(define (search_class_in_program class_name)
  (if (null? program_class)
      #f
      (for/or ([class_ program_class])
        (if (equal? class_name (car class_))
            (cdr class_)
            #f))
  )
)

(define (search_class class_name)
  (let ([classe (search_class_in_program class_name)])
    (if classe
        classe
        (raise-user-error "[ERROR] - Classe não encontrada: " class_name)
    )
  )
)

(define (print-program-class)
  (for ([class program_class])
    (displayln class)))

(define (merge_fields super-fields self-fields)
  (foldr (lambda (field acc)
           (if (member field acc) ; se já está na super-fields
               (append acc (list (string-append field "_old"))) ; então coloca um %1 no fim do nome para não dar conflito
               (append acc (list field)))) ; caso contrário, só adiciona no acumulador
        self-fields
        super-fields
  )
)

(define (create_methods m-decls nome_super fields)
  (define (create_method_aux m-decl)
    (list
     (ast:var-name (ast:method-name m-decl))
     (method (map ast:var-name (ast:method-params m-decl)) (ast:method-body m-decl) nome_super fields))
  )
  (append
   (map create_method_aux m-decls)
   (class-methods (search_class nome_super)))
)


; Execução do código começa aqui
(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
      (append_class "object" (class #f '() '()))
      (for ([decl decls])
       (let* ([nome (ast:var-name (ast:decl-name decl))] ; Nome da Classe
              [nome_super (ast:var-name (ast:decl-super decl))] ; Nome da Super-classe
              ; [fields_super (class-name_fields (search_class nome_super))]
              ; Nome Fields da classe atual e da super-classe
              [fields (merge_fields (class-name_fields (search_class nome_super)) (map ast:var-name (ast:decl-fields decl)))] 
              [metodos (create_methods (ast:decl-methods decl) nome_super fields)]
              [classe (class nome_super fields metodos)]
              )
              (append_class nome classe)
       )
      )
      (result-of stmt init-env)
)]))
