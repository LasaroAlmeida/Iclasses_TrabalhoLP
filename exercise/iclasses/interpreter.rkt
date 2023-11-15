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
    [(ast:var v) (apply-env Δ v)] ; esta implementação só funciona para variáveis imutáveis
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args) (display "send expression unimplemented")]
    [(ast:super (ast:var c) args) (display "super expression unimplemented")]
    [(ast:self) (display "self expression unimplemented")]
    [(ast:new (ast:var c) args) (display "new expression unimplemented")]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (display "assignment unimplemented")]
    [(ast:print e) (display (value-of e Δ))
                   #;(display "print unimplemented")]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (display "block unimplemented")]
    [(ast:if-stmt e s1 s2) (display "if statment unimplemented")]
    [(ast:while e s) (display "while unimplemented")]
    [(ast:local-decl (ast:var x) s) (display "local var declaration unimplemented")]
    [(ast:send e (ast:var mth) args) (display "command send unimplemented")]
    [(ast:super (ast:var c) args) (display "command super unimplemented")]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))


(define (append_class class-name classe)
  (if (already_exists_class? class-name classe)
      (raise-user-error "[ERROR] - A Classe já existe - " class-name)
      (begin
        (set! program_class (cons (cons class-name classe) program_class))
        (display "Classe adicionada: ")
        (display class-name)
        (newline)
      )
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
            #f))))

(define (search_class class_name)
  (let ([classe (search_class_in_program class_name)])
    (if classe
        classe
        (raise-user-error "[ERROR] - Classe não encontrada: " class_name)
    )
  )
)

(define (get_name_fields fields)
  (map ast:var-name fields))


(define (print-program-class)
  (for ([class program_class])
    (displayln class)))


(define (merge_fields fields super-fields)
  (map (lambda (field)
         (if (member field super-fields)
             (string-append field "$S")
             field))
       fields))

; (define (create_methods methods_decl super_name fields)
;   ()  
; )

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
              [fields (merge_fields (map ast:var-name (ast:decl-fields decl)) (class-name_fields (search_class nome_super)))] 

              [metodos (create_methods (ast:decl-methods decl) nome_super fields)]
              [classe (class nome_super fields metodos)]
              )
      ;  (print-program-class)
        ; (display (if (zero? 0) fields "TESTE"))
        (append_class nome classe)
       )
      )
       ;(result-of stmt init-env)
)]))
