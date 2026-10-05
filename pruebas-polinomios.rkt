#lang eopl
;Autores: Juan Felipe Aristizabal 2459364-3743, Juan Sebastian Huertas 2459505-3743

;; Taller 1 — Polinomios dispersos.
;; Parte 4: la misma batería de pruebas sobre las tres representaciones.

(require rackunit)
(require (prefix-in listas: "polinomios-listas.rkt"))
(require (prefix-in procs:  "polinomios-procedimientos.rkt"))
(require (prefix-in dt:     "polinomios-datatypes.rkt"))

;; Cada batería recibe las operaciones de una representación y se llama tres
;; veces, una por representación. Así las tres pasan exactamente las mismas
;; pruebas. Los resultados se comparan como lista de pares (coeficiente . exponente).

;; armar : (symbol -> polinomio) x (polinomio x numero x natural -> polinomio)
;;         x symbol x lista de pares -> polinomio
;; Arma un polinomio insertando los pares uno por uno.
(define armar
  (lambda (cero ins var pares)
    (if (null? pares)
        (cero var)
        (ins (armar cero ins var (cdr pares))
             (car (car pares))
             (cdr (car pares))))))

;; =========================================================================
;; Pruebas de polinomio-cero e insertar-termino (Juan Felipe)
;; =========================================================================

;; bateria-cero-insertar : string x cero x ins x ->pares -> void
;; Pruebas de polinomio-cero e insertar-termino para una representación.
(define bateria-cero-insertar
  (lambda (nombre cero ins ->pares)
    (with-check-info (('representacion nombre))

      ;; polinomio-cero
      (check-equal? (->pares (cero 'x)) '())
      (check-equal? (->pares (cero 'y)) '())
      (check-exn #rx"variable debe ser un simbolo"
                 (lambda () (cero "x")))

      ;; insertar en el polinomio nulo
      (check-equal? (->pares (ins (cero 'x) 7 0)) '((7 . 0)))
      (check-equal? (->pares (ins (cero 'x) -3/2 4)) '((-3/2 . 4)))

      ;; el orden queda de mayor a menor sin importar cómo se inserte
      (check-equal? (->pares (ins (ins (ins (cero 'x) 7 0) 4 5) -3/2 2))
                    '((4 . 5) (-3/2 . 2) (7 . 0)))
      (check-equal? (->pares (ins (ins (ins (cero 'x) 4 5) -3/2 2) 7 0))
                    '((4 . 5) (-3/2 . 2) (7 . 0)))

      ;; mismo exponente: se suman los coeficientes
      (check-equal? (->pares (ins (ins (cero 'x) 3 2) 4 2)) '((7 . 2)))
      (check-equal? (->pares (ins (ins (cero 'x) 1/3 1) 1/6 1)) '((1/2 . 1)))
      (check-equal? (->pares (ins (ins (cero 'x) 1/2 1) 1/2 1)) '((1 . 1)))

      ;; inserción que cancela un término
      (check-equal? (->pares (ins (ins (cero 'x) 5 2) -5 2)) '())
      (check-equal? (->pares (ins (armar cero ins 'x '((4 . 5) (3 . 2) (7 . 0))) -3 2))
                    '((4 . 5) (7 . 0)))
      (check-equal? (->pares (ins (armar cero ins 'x '((4 . 5) (3 . 2) (7 . 0))) -7 0))
                    '((4 . 5) (3 . 2)))

      ;; inserción con coeficiente 0: no cambia nada
      (check-equal? (->pares (ins (cero 'x) 0 3)) '())
      (check-equal? (->pares (ins (armar cero ins 'x '((4 . 5) (7 . 0))) 0 2))
                    '((4 . 5) (7 . 0)))
      (check-equal? (->pares (ins (armar cero ins 'x '((4 . 5) (7 . 0))) 0 5))
                    '((4 . 5) (7 . 0)))

      ;; errores
      (check-exn #rx"exponente"
                 (lambda () (ins (cero 'x) 1 -1)))
      (check-exn #rx"exponente"
                 (lambda () (ins (cero 'x) 1 3/2)))
      (check-exn #rx"coeficiente"
                 (lambda () (ins (cero 'x) 1.5 2)))
      (check-exn #rx"polinomio"
                 (lambda () (ins 'no-es-un-polinomio 1 2))))))

(bateria-cero-insertar "listas" listas:polinomio-cero listas:insertar-termino
                       listas:polinomio->pares)
(bateria-cero-insertar "procedimientos" procs:polinomio-cero procs:insertar-termino
                       procs:polinomio->pares)
(bateria-cero-insertar "datatypes" dt:polinomio-cero dt:insertar-termino
                       dt:polinomio->pares)

;; =========================================================================
;; Pruebas de sumar (solo datatypes) (Juan Felipe)
;; =========================================================================

(define p-suma (armar dt:polinomio-cero dt:insertar-termino 'x '((4 . 5) (-3/2 . 2) (7 . 0))))
(define q-suma (armar dt:polinomio-cero dt:insertar-termino 'x '((-4 . 5) (1/2 . 2) (2 . 1))))
(define menos-p (armar dt:polinomio-cero dt:insertar-termino 'x '((-4 . 5) (3/2 . 2) (-7 . 0))))

;; el ejemplo del enunciado: se cancela x^5 y se suman los de x^2
(check-equal? (dt:polinomio->pares (dt:sumar p-suma q-suma))
              '((-1 . 2) (2 . 1) (7 . 0)))
;; la suma es conmutativa
(check-equal? (dt:polinomio->pares (dt:sumar q-suma p-suma))
              '((-1 . 2) (2 . 1) (7 . 0)))
;; suma que cancela todo
(check-equal? (dt:polinomio->pares (dt:sumar p-suma menos-p)) '())
;; sumar el polinomio nulo no cambia nada
(check-equal? (dt:polinomio->pares (dt:sumar p-suma (dt:polinomio-cero 'x)))
              '((4 . 5) (-3/2 . 2) (7 . 0)))
(check-equal? (dt:polinomio->pares (dt:sumar (dt:polinomio-cero 'x) p-suma))
              '((4 . 5) (-3/2 . 2) (7 . 0)))
(check-equal? (dt:polinomio->pares (dt:sumar (dt:polinomio-cero 'x) (dt:polinomio-cero 'x))) '())
;; variables distintas: error
(check-exn #rx"misma variable"
           (lambda () (dt:sumar p-suma (dt:polinomio-cero 'y))))

;; =========================================================================
;; Pruebas de coeficiente-de y eliminar-termino (Juan Sebastian)
;; =========================================================================

;; Pendiente (compañero). Escribir aquí una batería con la misma forma que
;; bateria-cero-insertar:
;;
;;   (define bateria-coef-elim
;;     (lambda (nombre cero ins coef elim ->pares)
;;       (with-check-info (('representacion nombre))
;;         ... pruebas ...)))
;;
;; y llamarla una vez por representación:
;;
;;   (bateria-coef-elim "listas" listas:polinomio-cero listas:insertar-termino
;;                      listas:coeficiente-de listas:eliminar-termino listas:polinomio->pares)
;;   (bateria-coef-elim "procedimientos" procs:polinomio-cero ...)
;;   (bateria-coef-elim "datatypes" dt:polinomio-cero ...)
;;
;; Debe cubrir: casos funcionales de coeficiente-de y eliminar-termino, el
;; polinomio nulo como caso base (consultar y eliminar), y los errores de
;; exponente inexistente en las dos funciones.
