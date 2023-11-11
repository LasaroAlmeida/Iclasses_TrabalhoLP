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


(define (append-class class-name classe)
  (if (already_exists_class? class-name classe)
      (raise-user-error "Já existe uma classe com a mesma definição: " class-name)
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
    (for ([class_ program_class])
      (if (equal? class_name (car class_))
          class_
          #f
      )
    )
  )
)
;  Tem erro aqui :
(define (search_class nome)
  (let ([classe (search_class_in_program nome)])
    (display "teste") ; Isso vai imprimir "teste"
    ; Faça algo com a classe se ela for encontrada
    (if classe
        classe
        (raise-user-error "[ERROR] - Classe não encontrada"))))

; Execução do código começa aqui
(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
      (append-class "object" (class #f '() '()))
      (for ([decl decls])
       (let* ([nome (ast:var-name (ast:decl-name decl))]
              [nome_super (ast:var-name (ast:decl-super decl))]
              [fields_super (class-name_fields (search_class nome_super))]
       )
        (display (if (zero? 0) "fields_super" "TESTE"))
       )
        ; (display (ast:var-name (ast:decl-name decl)))
      )
       ;(result-of stmt init-env)
)]))

