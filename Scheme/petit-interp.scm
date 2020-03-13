#! /usr/bin/env gsi -:dar

(define parse-and-execute
  (lambda (inp)
    (parse inp execute)))

(define next-sym
  (lambda (inp cont)
    (cond ((null? inp)
           (cont inp 'EOI)) ;; retourner symbole EOI a la fin de l'input
          ((blanc? (@ inp))
           (next-sym ($ inp) cont)) ;; sauter les blancs
          (else
           (let ((c (@ inp)))
             (cond ((chiffre? c)   (symbol-int inp cont))
                   ((lettre? c)    (symbol-id inp cont))
                   ((char=? c #\() (cont ($ inp) 'LPAR))
                   ((char=? c #\)) (cont ($ inp) 'RPAR))
                   ((char=? c #\;) (cont ($ inp) 'SEMI))
                   (else
                    (syntax-error))))))))

(define @
  (lambda (inp)
    (if (null? inp) #\nul (car inp))))

(define $
  (lambda (inp)
    (if (null? inp) '() (cdr inp))))

(define syntax-error
  (lambda ()
    "syntax error\n"))

(define blanc?
  (lambda (c)
    (or (char=? c #\space) (char=? c #\newline) (char=? c #\tab))))

(define chiffre?
  (lambda (c)
    (and (char>=? c #\0) (char<=? c #\9))))

(define lettre?
  (lambda (c)
    (and (char>=? c #\a) (char<=? c #\z))))

(define symbol-int
  (lambda (inp cont)
    (symbol-int-aux inp cont 0)))

(define symbol-int-aux
  (lambda (inp cont n)
    (if (chiffre? (@ inp))
        (symbol-int-aux ($ inp)
                        cont
                        (+ (* 10 n) (- (char->integer (@ inp)) 48)))
        (cont inp n))))

(define symbol-id
  (lambda (inp cont)
    (symbol-id-aux inp cont '())))

(define symbol-id-aux
  (lambda (inp cont lst)
    (if (lettre? (@ inp))
        (symbol-id-aux ($ inp) cont (cons (@ inp) lst))
        (let ((id (list->string (reverse lst))))
          (cond ((string=? id "print")
                 (cont inp 'PRINT-SYM))
                (else
                 (cont inp id)))))))

(define expect
  (lambda (expected-sym inp cont)
    (next-sym inp
              (lambda (inp sym)
                (if (equal? sym expected-sym)
                    (cont inp)
                    (syntax-error))))))

(define parse
  (lambda (inp cont)
    (<program> inp ;; analyser un <program>
               (lambda (inp program)
                 (expect 'EOI ;; verifier qu'il n'y a rien apres
                         inp
                         (lambda (inp)
                           (cont program)))))))

(define <program>
  (lambda (inp cont)
    (<stat> inp cont))) ;; analyser un <stat>

(define <stat>
  (lambda (inp cont)
    (next-sym inp
              (lambda (inp2 sym)
                (case sym ;; determiner quel genre de <stat>
                  ((PRINT-SYM)
                   (<print_stat> inp2 cont))
                  (else
                   (<expr_stat> inp cont)))))))

(define <print_stat>
  (lambda (inp cont)
    (<paren_expr> inp ;; analyser un <paren_expr>
                  (lambda (inp expr)
                    (expect 'SEMI ;; verifier qu'il y a ";" apres
                            inp
                            (lambda (inp)
                              (cont inp
                                    (list 'PRINT expr))))))))

(define <paren_expr>
  (lambda (inp cont)
    (expect 'LPAR ;; doit debuter par "("
            inp
            (lambda (inp)
              (<expr> inp ;; analyser un <expr>
                      (lambda (inp expr)
                        (expect 'RPAR ;; doit etre suivi de ")"
                                inp
                                (lambda (inp)
                                  (cont inp
                                        expr)))))))))

(define <expr_stat>
  (lambda (inp cont)
    (<expr> inp ;; analyser un <expr>
            (lambda (inp expr)
              (expect 'SEMI ;; doit etre suivi de ";"
                      inp
                      (lambda (inp)
                        (cont inp
                              (list 'EXPR expr))))))))

(define <expr>
  (lambda (inp cont)
    (next-sym inp ;; verifier 1e symbole du <expr>
              (lambda (inp2 sym1)
                (next-sym inp2 ;; verifier 2e symbole du <expr>
                          (lambda (inp3 sym2)
                            (if (and (string? sym1) ;; combinaison "id =" ?
                                     (equal? sym2 'EQ))
                                (<expr> inp3
                                        (lambda (inp expr)
                                          (cont inp
                                                (list 'ASSIGN
                                                      sym1
                                                      expr))))
                                (<test> inp cont))))))))

(define <test>
  (lambda (inp cont)
    (<sum> inp cont)))

(define <sum>
  (lambda (inp cont)
    (<mult> inp cont)))

(define <mult>
  (lambda (inp cont)
    (<term> inp cont)))

(define <term>
  (lambda (inp cont)
    (next-sym inp ;; verifier le premier symbole du <term>
              (lambda (inp2 sym)
                (cond ((string? sym) ;; identificateur?
                       (cont inp2 (list 'VAR sym)))
                      ((number? sym) ;; entier?
                       (cont inp2 (list 'INT sym)))
                      (else
                       (<paren_expr> inp cont)))))))

(define execute
  (lambda (ast)
    (exec-stat '() ;; etat des variables globales
               ""  ;; sortie jusqu'a date
               ast ;; ASA du programme
               (lambda (env output)
                 output)))) ;; retourner l'output pour qu'il soit affiche

(define exec-stat
  (lambda (env output ast cont)
    (case (car ast)

      ((PRINT)
       (exec-expr env ;; evaluer l'expression du print
                  output
                  (cadr ast)
                  (lambda (env output val)
                    (cont env ;; ajouter le resultat a la sortie
                          (string-append output
                                         (number->string val)
                                         "\n")))))

      ((EXPR)
       (exec-expr env ;; evaluer l'expression
                  output
                  (cadr ast)
                  (lambda (env output val)
                    (cont env output)))) ;; continuer en ignorant le resultat

      (else
       "internal error (unknown statement AST)\n"))))

(define exec-expr
  (lambda (env output ast cont)
    (case (car ast)

      ((INT)
       (cont env
             output
             (cadr ast))) ;; retourner la valeur de la constante
                    
      (else
       "internal error (unknown expression AST)\n"))))

(define main
  (lambda ()
    (print (parse-and-execute (read-all (current-input-port) read-char)))))
