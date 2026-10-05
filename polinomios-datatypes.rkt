#lang eopl
;Autores: Juan Felipe Aristizabal 2459364-3743, Juan Sebastian Huertas 2459505-3743

;; Taller 1 — Polinomios dispersos.
;; Parte 3: representación con datatypes.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio
;;   sumar             : polinomio x polinomio -> polinomio

(provide
 ;; tipos, predicados de tipo y constructores
 polinomio variable terminos termino-tad coeficiente exponente
 polinomio? variable? terminos? termino-tad? coeficiente? exponente?
 poli nombre-var sin-terminos mas-terminos termino coef-ent coef-rac expo-nat
 ;; interfaz del TAD
 polinomio-cero insertar-termino coeficiente-de eliminar-termino sumar
 ;; auxiliar de presentación (no pertenece a la interfaz del enunciado)
 polinomio->pares)

;; --- Gramática con define-datatype ---
;; Cada no terminal es un tipo y cada constructor del enunciado es una variante.
;; <termino> se llama termino-tad porque define-datatype no deja que el tipo
;; tenga el mismo nombre que su variante (que sigue llamándose termino).
;; Ojo: en las funciones con cases ningún parámetro puede llamarse como un tipo.

;; <variable> ::= nombre-var(s)
(define-datatype variable variable?
  (nombre-var (s symbol?)))

;; <exponente> ::= expo-nat(k)
(define-datatype exponente exponente?
  (expo-nat (k integer?)))

;; <coeficiente> ::= coef-ent(n) | coef-rac(num, den)
(define-datatype coeficiente coeficiente?
  (coef-ent (n integer?))
  (coef-rac (num integer?) (den integer?)))

;; <termino> ::= termino(coef, expo)
(define-datatype termino-tad termino-tad?
  (termino (coef coeficiente?) (expo exponente?)))

;; <terminos> ::= sin-terminos() | mas-terminos(term, resto)
(define-datatype terminos terminos?
  (sin-terminos)
  (mas-terminos (term termino-tad?) (resto terminos?)))

;; <polinomio> ::= poli(var, terms)
(define-datatype polinomio polinomio?
  (poli (var variable?) (terms terminos?)))

;; --- Traducción concreto <-> abstracto ---

;; exponente-concreto? : any -> boolean
;; Dice si v es un entero exacto mayor o igual a 0.
(define exponente-concreto?
  (lambda (v)
    (and (integer? v) (exact? v) (>= v 0))))

;; coeficiente-concreto? : any -> boolean
;; Dice si v es un número exacto, entero o fracción (4, -3/2).
(define coeficiente-concreto?
  (lambda (v)
    (and (rational? v) (exact? v))))

;; concreto->coeficiente : numero -> coeficiente
;; Pasa un número de Racket a coeficiente: 4 -> (coef-ent 4), -3/2 -> (coef-rac -3 2).
(define concreto->coeficiente
  (lambda (n)
    (if (integer? n)
        (coef-ent n)
        (coef-rac (numerator n) (denominator n)))))

;; coeficiente->concreto : coeficiente -> numero
;; Hace lo contrario: (coef-ent 4) -> 4, (coef-rac -3 2) -> -3/2.
(define coeficiente->concreto
  (lambda (c)
    (cases coeficiente c
      (coef-ent (n) n)
      (coef-rac (num den) (/ num den)))))

;; crear-termino : numero x natural -> termino-tad
;; Arma un término con un coeficiente y un exponente de Racket.
(define crear-termino
  (lambda (c e)
    (termino (concreto->coeficiente c) (expo-nat e))))

;; termino->coeficiente-concreto : termino-tad -> numero
;; Devuelve el coeficiente del término como número de Racket.
(define termino->coeficiente-concreto
  (lambda (t)
    (cases termino-tad t
      (termino (coef expo) (coeficiente->concreto coef)))))

;; termino->exponente-concreto : termino-tad -> natural
;; Devuelve el exponente del término como número de Racket.
(define termino->exponente-concreto
  (lambda (t)
    (cases termino-tad t
      (termino (coef expo)
        (cases exponente expo
          (expo-nat (k) k))))))

;; variable->simbolo : variable -> symbol
;; Devuelve el símbolo de la variable.
(define variable->simbolo
  (lambda (v)
    (cases variable v
      (nombre-var (s) s))))

;; --- polinomio-cero ---

;; polinomio-cero : symbol -> polinomio
;; Devuelve el polinomio 0 en esa variable. Da error si no es un símbolo.
(define polinomio-cero
  (lambda (v)
    (if (symbol? v)
        (poli (nombre-var v) (sin-terminos))
        (eopl:error 'polinomio-cero "La variable debe ser un simbolo"))))

;; --- insertar-termino ---

;; insertar-en-terminos : terminos x numero x natural -> terminos
;; Recorre la lista una sola vez. Si el exponente actual es mayor, sigue con el
;; resto; si es menor, el término nuevo va antes; si es igual, suma los
;; coeficientes (si da 0 quita el término).
(define insertar-en-terminos
  (lambda (ts c e)
    (cases terminos ts
      (sin-terminos ()
        (mas-terminos (crear-termino c e) ts))
      (mas-terminos (actual resto)
        (let ((k (termino->exponente-concreto actual)))
          (if (> k e)
              (mas-terminos actual (insertar-en-terminos resto c e))
              (if (< k e)
                  (mas-terminos (crear-termino c e) ts)
                  (let ((suma (+ (termino->coeficiente-concreto actual) c)))
                    (if (zero? suma)
                        resto
                        (mas-terminos (crear-termino suma e) resto))))))))))

;; insertar-termino : polinomio x numero x natural -> polinomio
;; Inserta el término en su lugar. Si el exponente ya está, suma los
;; coeficientes (si da 0 quita el término). Con coeficiente 0 no cambia nada.
;; Da error si el exponente no es un entero >= 0 o el coeficiente no es exacto.
(define insertar-termino
  (lambda (p c e)
    (if (not (exponente-concreto? e))
        (eopl:error 'insertar-termino "El exponente debe ser un entero no negativo")
        (if (not (coeficiente-concreto? c))
            (eopl:error 'insertar-termino "El coeficiente debe ser un numero exacto")
            (if (not (polinomio? p))
                (eopl:error 'insertar-termino "El primer argumento debe ser un polinomio")
                (if (zero? c)
                    p
                    (cases polinomio p
                      (poli (var terms)
                        (poli var (insertar-en-terminos terms c e))))))))))

;; --- coeficiente-de y eliminar-termino ---

;; coeficiente-de : polinomio x natural -> numero
;; Devuelve el coeficiente del término con ese exponente. Da error si no existe.
(define coeficiente-de
  (lambda (p e)
    (eopl:error 'coeficiente-de "Sin implementar")))

;; eliminar-termino : polinomio x natural -> polinomio
;; Devuelve el polinomio sin el término con ese exponente. Da error si no existe.
(define eliminar-termino
  (lambda (p e)
    (eopl:error 'eliminar-termino "Sin implementar")))

;; --- sumar ---

;; sumar : polinomio x polinomio -> polinomio
;; Suma dos polinomios de la misma variable. Da error si las variables son distintas.
(define sumar
  (lambda (p q)
    (eopl:error 'sumar "Sin implementar")))

;; --- Auxiliar de presentación ---

;; pares-de-terminos : terminos -> lista de pares (coeficiente . exponente)
;; Recorre la lista de términos y arma los pares.
(define pares-de-terminos
  (lambda (ts)
    (cases terminos ts
      (sin-terminos () '())
      (mas-terminos (t resto)
        (cons (cons (termino->coeficiente-concreto t)
                    (termino->exponente-concreto t))
              (pares-de-terminos resto))))))

;; polinomio->pares : polinomio -> lista de pares (coeficiente . exponente)
;; Muestra el polinomio como lista de pares, de mayor a menor exponente.
;; Sirve para ver los resultados; no es parte de la interfaz.
(define polinomio->pares
  (lambda (p)
    (cases polinomio p
      (poli (var terms)
        (pares-de-terminos terms)))))

;; ---- Ejemplos de construcción ----

;; 1. Polinomio nulo en x.
(define ejemplo-nulo
  (poli (nombre-var 'x) (sin-terminos)))
;; (polinomio? ejemplo-nulo)                         => #t
;; (polinomio->pares ejemplo-nulo)                   => ()

;; 2. 7x^3
(define ejemplo-un-termino
  (poli (nombre-var 'x)
        (mas-terminos (termino (coef-ent 7) (expo-nat 3))
                      (sin-terminos))))
;; (terminos? (mas-terminos (termino (coef-ent 7) (expo-nat 3)) (sin-terminos)))   => #t
;; (polinomio->pares ejemplo-un-termino)             => ((7 . 3))

;; 3. (3/4)x^5 - 2x
(define ejemplo-racional
  (poli (nombre-var 'x)
        (mas-terminos (termino (coef-rac 3 4) (expo-nat 5))
          (mas-terminos (termino (coef-ent -2) (expo-nat 1))
            (sin-terminos)))))
;; (coeficiente? (coef-rac 3 4))                     => #t
;; (polinomio->pares ejemplo-racional)               => ((3/4 . 5) (-2 . 1))

;; 4. 4x^5 - (3/2)x^2 + 7
(define ejemplo-tres-terminos
  (poli (nombre-var 'x)
        (mas-terminos (termino (coef-ent 4) (expo-nat 5))
          (mas-terminos (termino (coef-rac -3 2) (expo-nat 2))
            (mas-terminos (termino (coef-ent 7) (expo-nat 0))
              (sin-terminos))))))
;; (polinomio->pares ejemplo-tres-terminos)          => ((4 . 5) (-3/2 . 2) (7 . 0))

;; 5. -x^2 + 2x + 7 en la variable y.
(define ejemplo-en-y
  (poli (nombre-var 'y)
        (mas-terminos (termino (coef-ent -1) (expo-nat 2))
          (mas-terminos (termino (coef-ent 2) (expo-nat 1))
            (mas-terminos (termino (coef-ent 7) (expo-nat 0))
              (sin-terminos))))))
;; (variable? (nombre-var 'y))                       => #t
;; (polinomio->pares ejemplo-en-y)                   => ((-1 . 2) (2 . 1) (7 . 0))

;; ---- Ejemplos de polinomio-cero ----

;; 1. Polinomio nulo en x.
(define cero-x (polinomio-cero 'x))
;; (polinomio->pares cero-x)                         => ()

;; 2. Polinomio nulo en y.
(define cero-y (polinomio-cero 'y))
;; (polinomio? cero-y)                               => #t
;; (polinomio->pares cero-y)                         => ()

;; 3. Error: la variable debe ser un símbolo.
;; (polinomio-cero "x")   => error: La variable debe ser un simbolo

;; ---- Ejemplos de insertar-termino ----

;; p = 4x^5 - (3/2)x^2 + 7, armado solo con la interfaz.
(define p
  (insertar-termino
   (insertar-termino
    (insertar-termino (polinomio-cero 'x) 7 0)
    -3/2 2)
   4 5))
;; (polinomio->pares p)                              => ((4 . 5) (-3/2 . 2) (7 . 0))

;; 1. Exponente nuevo, queda en el medio.
;; (polinomio->pares (insertar-termino p 2 3))       => ((4 . 5) (2 . 3) (-3/2 . 2) (7 . 0))

;; 2. El exponente ya existe: se suman los coeficientes.
;; (polinomio->pares (insertar-termino p 1 2))       => ((4 . 5) (-1/2 . 2) (7 . 0))

;; 3. La suma da cero: el término desaparece.
;; (polinomio->pares (insertar-termino p 3/2 2))     => ((4 . 5) (7 . 0))

;; 4. Coeficiente 0: el polinomio no cambia.
;; (polinomio->pares (insertar-termino p 0 2))       => ((4 . 5) (-3/2 . 2) (7 . 0))

;; 5. Error: exponente negativo.
;; (insertar-termino p 5 -1)  => error: El exponente debe ser un entero no negativo

;; 6. Error: coeficiente no exacto.
;; (insertar-termino p 1.5 2) => error: El coeficiente debe ser un numero exacto
