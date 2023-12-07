#lang racket

; Dener Luis Basílio Teodoro 201835001
; Lásaro de Almeida Deodoro  201835004

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

(struct object (class_name fields))

(struct class (super_name name_fields methods))
(struct method (arguments body super_name fields))

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
    [(ast:var v) (deref (apply-env Δ v))] ; pega o endereço da variável v no ambiente e acessa seu conteúdo em memória
    ; avalia e1 no ambiente
    ; aloca memória para o value de e1
    ; extende o ambiente adicionando o endereço de memória 
    ; avalia e2 no ambiente extendido
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (newref (value-of e1 Δ)) Δ))] 
    [(ast:send e (ast:var mth) args) 
                (let* ([value_args (apply-value-of args Δ)]
                      [objeto (value-of e Δ)])
                      (apply_method (search_method (object-class_name objeto) mth) objeto value_args)
                )]
    [(ast:super (ast:var c) args) (let ([value_args (apply-value-of args Δ)] ; busca o valor dos argumentos no ambiente. 
                      [objeto (apply-env Δ "self")]) ; obtem o objeto a partir do ambiente
                      ; pega o nome da super classe;
                      ; 
                      (apply_method (search_method (apply-env Δ "super")  (ast:var-name args)) objeto value_args)
                )]

    [(ast:self) (apply-env Δ "self")] ; busca o objeto associado ao self dentro do ambiente
    [(ast:new (ast:var c) args) ; c é o nome do classe instanciada
      (let* ([value_args (apply-value-of args Δ)] ; Avalia cada um dos argumentos recebidos
              [objeto (new_object c value_args)] ; cria um novo objeto. nome da classe e lista de endereços de memória. ex (1 2)
            )
            ; busca pelo método initialize
            ; aplica o método com o objeto e os valores dos argumentos.
            ; tem-se então a implementação do médoto, o objeto com o nome da classe e a lista de endereços de memória para os fields, e os valores dos argumentos
            (apply_method (search_method c "initialize") objeto value_args)
            ; retorna o objeto
            objeto
      )]
    [e (raise-user-error "unimplemented-construction: " e)]))


; aplica um método no objeto atual e seus argumentos
; args são os valores dos argumentos
(define (apply_method method self args)
    ; cria uma referencia em memória para cada argumentos
  (let* ([args-refs (map newref args)] ;
        ; pega o nome da classe super 
        ; extende um ambiente vazio associando super ao nome da classe super
        ; extende o ambiente novamente associando "self" ao objeto
        ; o ambiente de classe é criado
         [class_env (extend-env "self" self (extend-env "super" (method-super_name method) empty-env))]
         ; (method-fields method) pega os nomes dos fields. ex (i j)
         ; (object-fields self) pega os endereços de memória destinados aos fiels e que estão com null
         ; cria um ambiente que com os nomes dos fields associados a seus endereços em memória
         [method_env (bind-vars (method-fields method) (object-fields self) class_env)]
         ; pega o nome dos argumentos do método. Ex. count(x) -> o agumento é o x
         ; usa o bind-var para associar os nomes dos argumentos às referencias em memória para seus valores
         ; o ambiente é novamente extendido e agora associa os nomes dos argumentos a seus valores em memória
         [method_env_vars (bind-vars (method-arguments method) args-refs method_env)]
          )
          ; faz o result of do corpo do método a partir do ambiente da classe
    (result-of (method-body method) method_env_vars))
)

; faz associação do vars com values no enviente
; por exemplo, receve a lista de nome de fields e a lista de endereços de memória para eles
; associa o nome ao endereço extendendo o ambiente
(define (bind-vars vars values env)
  (for ([var vars] [val values])
    (set! env (extend-env var val env))
  )
  env
)

(define (apply-value-of exps Δ)
  (map (lambda (exp) (value-of exp Δ)) exps))

(define (new_object class_name value_args)
    ; Busca a classe a partir do nome
    ; Busca os nomes dos fields dessa classe. Exe: (i, j)
    (let* ([name_fields (class-name_fields (search_class class_name))]
        ;  para cada nome de campo, é criado uma referencia em memória contendo null
           [fields (map (lambda (f_name) (newref null)) name_fields)]
          )
        ;   cria o nome objeto com nome da classe e a lista de referencias para os campos do objeto incializados com null.
          (object class_name fields)
    )
)

;
(define (search_method class_name method_name)
    ; busca a classe
    ; pega os metodos disponíveis da classe
    (let ([methods (class-methods (search_class class_name))])
    ; tenta associar o method_name com o algum dos métodos presentes na classe
      (let ([found-method (assoc method_name methods)])
        (if found-method ; se o método existe
            (cadr found-method) ; retorna o objeto método
            (raise-user-error "[ERROR] - Método não encontrado: " method_name)
        )
      )
    )
)

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e) (begin 
                                  (setref! (apply-env Δ x) (value-of e Δ)) ;Atribui o valor de e na variável x; retorna a referencia para x. Atualiza o valor associada a referencia
                                  42)] ; 
    [(ast:print e) (display (value-of e Δ)) 
                          (newline)]; faz nova linha após o display
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts) (for ([l stmts])
                             (result-of l Δ) ; para cada stmts, faz o result-of
                            )]
    [(ast:if-stmt e s1 s2) (if (value-of e Δ) ;Se verdadeiro o value-of de e no ambiente
                                (result-of s1 Δ) ; Resolve o stmt s1 no ambiente
                                (result-of s2 Δ))] ; resoolver o s2 no ambiente
    [(ast:while e s) (if (value-of e Δ) ;Avalia o valor de e no ambiente. Se a condição do while é verdadeira
                          (begin
                            (result-of s Δ) ; avalia o corpo do while
                            (result-of stmt Δ) ; Para a nova avaliação do while
                          )
                          #f ; retorna falo, o while deu falso
                      )]
    [(ast:local-decl (ast:var x) s)  (result-of s (extend-env x (newref 'empty) Δ))] ; cria um referencia vazia em memória;
                                                                                     ; extende o ambiene adicionando x e sua referencia;
                                                                                     ; aplica o result of no novo ambiente extendido.
                                                                                     ; Basicamente cria um local em memória para a variável
    [(ast:send e (ast:var mth) args) 
                ; faz o value-of de cada argumentos e pega os valores deles
                (let* ([value_args (apply-value-of args Δ)]
                      [objeto (value-of e Δ)]) ; obtem o objeto correspondente
                    ;  obtem o nome da classe do objeto
                    ; busca o método mth na classe
                    ; aplica o meétodo passando o objeto atual e os valores dos argumentos
                    (apply_method (search_method (object-class_name objeto) mth) objeto value_args)
                )]

    [(ast:super (ast:var c) args) (let ([value_args (apply-value-of args Δ)] ; obtem os valores de dos argumentos do método
                      [objeto (apply-env Δ "self")]) ; obtem o objeto a partir do ambiente
                      ;obtem p nome dda classse super
                      ; busca o método c na classe
                      ; aplica o método

                      (apply_method (search_method (apply-env Δ "super") c) objeto value_args)
                )]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))


; Função responsável por adicionar uma nova classe ao program_class
(define (append_class class-name classe)
  (if (already_exists_class? class-name classe) ;se a classe já existe
      (raise-user-error "[ERROR] - A Classe já existe - " class-name)
      (set! program_class (cons (cons class-name classe) program_class)) ;adiciona a classe na lista
  )
)

; 
(define (already_exists_class? class_name classe)
  (let ([old_class (search_class_in_program class_name)]) ;Pega a classe se já existe uma de mesmo nome
    (and old_class ;Se ela existe
      (equal? (class-methods old_class) (class-methods classe)) ;Seus methos são iguais aos metótos da nova classe
      (equal? (class-name_fields old_class) (class-name_fields classe)) ;Seus fields são iguais aos  da nova classe
    ;   então a classe já existe ou nao
    )
  )
)

; Busca uma classe no program_class a partir do class_name
(define (search_class_in_program class_name)
  (if (null? program_class) ;Se program_class é null
      #f
    ;   for/or serve para retornar o primeiro valor verdadeiro ou então falso
      (for/or ([class_ program_class])
        (if (equal? class_name (car class_)) ; Se o class_ name é igual ao car de class_
            (cdr class_) ; retorna o clase
            #f)) ; retorna falso
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

;Dados os fields da super classe e da classe atual
(define (merge_fields super-fields self-fields)
  (foldr (lambda (field acc)
           (if (member field acc) 
               (append acc (list (string-append field "_old"))); adiciona _old na nome do campo que já existia na classe pai. Poderíamos tê-lo retirado também
               (append acc (list field)))) 
        self-fields
        super-fields
  )
)

; cria um metodo
(define (create_methods m-decls nome_super fields)
  (define (create_method_aux m-decl) ;auxila na criação de métodos
    (list ;cria uma lista contendo nome do metodo / 
     (ast:var-name (ast:method-name m-decl))
     ; cria um método. Ele contem os nomes dos parametros, corpo, nome da super classe e os nomes dos fields que possui acesso.
     (method (map ast:var-name (ast:method-params m-decl)) (ast:method-body m-decl) nome_super fields))
  )
  (append ;combina os métodos da classe atual com os da superclasse
   (map create_method_aux m-decls) ;aplica a função para cada método
   (class-methods (search_class nome_super))) ; pega os métodos da super classe
)


(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
      (append_class "object" (class #f '() '())) ;Adiciona a classe object ao program_class. É a principal classe, é a classe a qual todas herdam
      (for ([decl decls]) ;para cada declaração de classe na lista de declarações
       (let* ([nome (ast:var-name (ast:decl-name decl))] ;pega o nome da classe
              [nome_super (ast:var-name (ast:decl-super decl))] ;pega o nome da super_classe dela

              ;pega a lista de nomes dos fields;
              ;pega os fields da super_class
              ; une os fields das duas classes
              [fields (merge_fields (class-name_fields (search_class nome_super)) (map ast:var-name (ast:decl-fields decl)))] 

              ;pega os nomes dos métodos
              ;cria os métodos com base em seus nomes, nome da classe super e os nomes dos fields
              [metodos (create_methods (ast:decl-methods decl) nome_super fields)]
              ; cria uma classe com base na super_classe, nos fields e nos métodos
              [classe (class nome_super fields metodos)]
              )
            ;   adiciona classe na lista de classes
              (append_class nome classe)
       )
      )
      (result-of stmt init-env)
)]))
