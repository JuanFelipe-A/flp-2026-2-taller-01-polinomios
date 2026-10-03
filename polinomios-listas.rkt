#lang eopl
;Autores: Juan Felipe Aristizabal 2459364-3743, Juan Sebastian Huertas 2459505-3743

;; Taller 1 — Polinomios dispersos.
;; Parte 1: representación basada en listas.
;;
;; Interfaz del TAD. Cada función va comentada con su nombre, su contrato
;; (entrada -> salida) y su propósito, y ninguna recorre la lista de términos
;; más de una vez ni la ordena al final.
;;
;;   polinomio-cero    : symbol -> polinomio
;;   insertar-termino  : polinomio x coeficiente x exponente -> polinomio
;;   coeficiente-de    : polinomio x exponente -> coeficiente
;;   eliminar-termino  : polinomio x exponente -> polinomio

(provide
 poli nombre-var sin-terminos mas-terminos termino coef-ent coef-rac expo-nat
 poli? nombre-var? sin-terminos? mas-terminos? termino? coef-ent? coef-rac? expo-nat?
 poli->var poli->terms nombre-var->s mas-terminos->term mas-terminos->resto
 termino->coef termino->expo coef-ent->n coef-rac->num coef-rac->den expo-nat->k
 polinomio-cero insertar-termino coeficiente-de eliminar-termino
 polinomio->pares)

;; Representación con listas: cada variante es una lista cuyo primer
;; elemento es la etiqueta.
;;   (poli <nombre-var> <terminos>)        (nombre-var <symbol>)
;;   (sin-terminos)                        (mas-terminos <termino> <terminos>)
;;   (termino <coeficiente> <exponente>)   (coef-ent <int>)
;;   (coef-rac <int> <int>)                (expo-nat <int>)

;; es-variante? : any x symbol x int -> boolean
;; Verdadero si x es una lista de n elementos que empieza con la etiqueta dada.
(define es-variante?
  (lambda (x etiqueta n)
    (and (list? x)
         (= (length x) n)
         (eq? (car x) etiqueta))))

;; extraer : symbol x (any -> boolean) x symbol x (list -> any) x any -> any
;; Aplica el selector al dato si el predicado lo acepta; si no, levanta un error.
(define extraer
  (lambda (quien variante? variante seleccionar dato)
    (if (variante? dato)
        (seleccionar dato)
        (eopl:error quien "Se esperaba un dato de la variante ~s y se recibio ~s"
                    variante dato))))

;; --- Constructores ---

;; poli : nombre-var x terminos -> polinomio
;; Construye un polinomio con su variable y su lista de términos.
(define poli
  (lambda (var terms)
    (list 'poli var terms)))

;; nombre-var : symbol -> nombre-var
;; Construye la variable del polinomio.
(define nombre-var
  (lambda (s)
    (list 'nombre-var s)))

;; sin-terminos : () -> terminos
;; Construye la lista de términos vacía.
(define sin-terminos
  (lambda ()
    (list 'sin-terminos)))

;; mas-terminos : termino x terminos -> terminos
;; Construye una lista de términos con term al frente y resto detrás.
(define mas-terminos
  (lambda (term resto)
    (list 'mas-terminos term resto)))

;; termino : coeficiente x exponente -> termino
;; Construye un término.
(define termino
  (lambda (coef expo)
    (list 'termino coef expo)))

;; coef-ent : int -> coeficiente
;; Construye un coeficiente entero.
(define coef-ent
  (lambda (n)
    (list 'coef-ent n)))

;; coef-rac : int x int -> coeficiente
;; Construye un coeficiente racional con numerador y denominador.
(define coef-rac
  (lambda (num den)
    (list 'coef-rac num den)))

;; expo-nat : int -> exponente
;; Construye un exponente.
(define expo-nat
  (lambda (k)
    (list 'expo-nat k)))

;; --- Predicados ---

;; poli? : any -> boolean
;; Verdadero si x fue construido con poli.
(define poli?
  (lambda (x) (es-variante? x 'poli 3)))

;; nombre-var? : any -> boolean
;; Verdadero si x fue construido con nombre-var.
(define nombre-var?
  (lambda (x) (es-variante? x 'nombre-var 2)))

;; sin-terminos? : any -> boolean
;; Verdadero si x fue construido con sin-terminos.
(define sin-terminos?
  (lambda (x) (es-variante? x 'sin-terminos 1)))

;; mas-terminos? : any -> boolean
;; Verdadero si x fue construido con mas-terminos.
(define mas-terminos?
  (lambda (x) (es-variante? x 'mas-terminos 3)))

;; termino? : any -> boolean
;; Verdadero si x fue construido con termino.
(define termino?
  (lambda (x) (es-variante? x 'termino 3)))

;; coef-ent? : any -> boolean
;; Verdadero si x fue construido con coef-ent.
(define coef-ent?
  (lambda (x) (es-variante? x 'coef-ent 2)))

;; coef-rac? : any -> boolean
;; Verdadero si x fue construido con coef-rac.
(define coef-rac?
  (lambda (x) (es-variante? x 'coef-rac 3)))

;; expo-nat? : any -> boolean
;; Verdadero si x fue construido con expo-nat.
(define expo-nat?
  (lambda (x) (es-variante? x 'expo-nat 2)))

;; --- Extractores ---

;; poli->var : polinomio -> nombre-var
;; Devuelve la variable del polinomio.
(define poli->var
  (lambda (p) (extraer 'poli->var poli? 'poli cadr p)))

;; poli->terms : polinomio -> terminos
;; Devuelve la lista de términos del polinomio.
(define poli->terms
  (lambda (p) (extraer 'poli->terms poli? 'poli caddr p)))

;; nombre-var->s : nombre-var -> symbol
;; Devuelve el símbolo de la variable.
(define nombre-var->s
  (lambda (v) (extraer 'nombre-var->s nombre-var? 'nombre-var cadr v)))

;; mas-terminos->term : terminos -> termino
;; Devuelve el primer término de una lista no vacía.
(define mas-terminos->term
  (lambda (ts) (extraer 'mas-terminos->term mas-terminos? 'mas-terminos cadr ts)))

;; mas-terminos->resto : terminos -> terminos
;; Devuelve la lista sin su primer término.
(define mas-terminos->resto
  (lambda (ts) (extraer 'mas-terminos->resto mas-terminos? 'mas-terminos caddr ts)))

;; termino->coef : termino -> coeficiente
;; Devuelve el coeficiente del término.
(define termino->coef
  (lambda (t) (extraer 'termino->coef termino? 'termino cadr t)))

;; termino->expo : termino -> exponente
;; Devuelve el exponente del término.
(define termino->expo
  (lambda (t) (extraer 'termino->expo termino? 'termino caddr t)))

;; coef-ent->n : coeficiente -> int
;; Devuelve el entero de un coef-ent.
(define coef-ent->n
  (lambda (c) (extraer 'coef-ent->n coef-ent? 'coef-ent cadr c)))

;; coef-rac->num : coeficiente -> int
;; Devuelve el numerador de un coef-rac.
(define coef-rac->num
  (lambda (c) (extraer 'coef-rac->num coef-rac? 'coef-rac cadr c)))

;; coef-rac->den : coeficiente -> int
;; Devuelve el denominador de un coef-rac.
(define coef-rac->den
  (lambda (c) (extraer 'coef-rac->den coef-rac? 'coef-rac caddr c)))

;; expo-nat->k : exponente -> int
;; Devuelve el entero de un expo-nat.
(define expo-nat->k
  (lambda (e) (extraer 'expo-nat->k expo-nat? 'expo-nat cadr e)))

;; ---- Interfaz: inicio (igual en listas y procedimientos) ----
;; Solo usa constructores, predicados y extractores; nunca car, cdr ni list.

;; exponente-concreto? : any -> boolean
;; Verdadero si v es un entero exacto mayor o igual que 0.
(define exponente-concreto?
  (lambda (v)
    (and (integer? v) (exact? v) (>= v 0))))

;; coeficiente-concreto? : any -> boolean
;; Verdadero si v es un número racional exacto (4, -3/2).
(define coeficiente-concreto?
  (lambda (v)
    (and (rational? v) (exact? v))))

;; concreto->coeficiente : numero -> coeficiente
;; Traduce 4 a (coef-ent 4) y -3/2 a (coef-rac -3 2).
(define concreto->coeficiente
  (lambda (n)
    (if (integer? n)
        (coef-ent n)
        (coef-rac (numerator n) (denominator n)))))

;; coeficiente->concreto : coeficiente -> numero
;; Camino inverso: (coef-ent 4) da 4 y (coef-rac -3 2) da -3/2.
(define coeficiente->concreto
  (lambda (c)
    (if (coef-ent? c)
        (coef-ent->n c)
        (/ (coef-rac->num c) (coef-rac->den c)))))

;; crear-termino : numero x natural -> termino
;; Arma un término a partir de un coeficiente y un exponente concretos.
(define crear-termino
  (lambda (coeficiente exponente)
    (termino (concreto->coeficiente coeficiente) (expo-nat exponente))))

;; polinomio-cero : symbol -> polinomio
;; Devuelve el polinomio nulo en la variable dada. Error si no es un símbolo.
(define polinomio-cero
  (lambda (variable)
    (if (symbol? variable)
        (poli (nombre-var variable) (sin-terminos))
        (eopl:error 'polinomio-cero "La variable debe ser un simbolo"))))

;; insertar-termino : polinomio x numero x natural -> polinomio
;; Devuelve el polinomio con el término insertado en su posición. Si el
;; exponente ya existe suma los coeficientes (y quita el término si da 0); con
;; coeficiente 0 no cambia nada. Error si el exponente no es un entero no
;; negativo o si el coeficiente no es un número exacto.
(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (eopl:error 'insertar-termino "Sin implementar")))

;; coeficiente-de : polinomio x natural -> numero
;; Devuelve el coeficiente del término con ese exponente. Error si no existe.
(define coeficiente-de
  (lambda (polinomio exponente)
    (eopl:error 'coeficiente-de "Sin implementar")))

;; eliminar-termino : polinomio x natural -> polinomio
;; Devuelve el polinomio sin el término con ese exponente. Error si no existe.
(define eliminar-termino
  (lambda (polinomio exponente)
    (eopl:error 'eliminar-termino "Sin implementar")))

;; polinomio->pares : polinomio -> lista de pares (coeficiente . exponente)
;; Muestra el polinomio como datos de Racket, de mayor a menor exponente.
;; No es parte de la interfaz; se usa en los ejemplos y en las pruebas.
(define polinomio->pares
  (lambda (polinomio)
    (letrec ((pares
              (lambda (terminos)
                (if (sin-terminos? terminos)
                    '()
                    (let ((t (mas-terminos->term terminos)))
                      (cons (cons (coeficiente->concreto (termino->coef t))
                                  (expo-nat->k (termino->expo t)))
                            (pares (mas-terminos->resto terminos))))))))
      (pares (poli->terms polinomio)))))

;; ---- Interfaz: fin ----

;; ---- Ejemplos de construcción y observadores ----

;; 1. Polinomio nulo en x.
(define ejemplo-nulo
  (poli (nombre-var 'x) (sin-terminos)))
;; (poli? ejemplo-nulo)                              => #t
;; (sin-terminos? (poli->terms ejemplo-nulo))        => #t
;; (nombre-var->s (poli->var ejemplo-nulo))          => x

;; 2. 7x^3
(define ejemplo-un-termino
  (poli (nombre-var 'x)
        (mas-terminos (termino (coef-ent 7) (expo-nat 3))
                      (sin-terminos))))
;; (mas-terminos? (poli->terms ejemplo-un-termino))  => #t
;; (coef-ent->n (termino->coef
;;   (mas-terminos->term (poli->terms ejemplo-un-termino))))   => 7
;; (expo-nat->k (termino->expo
;;   (mas-terminos->term (poli->terms ejemplo-un-termino))))   => 3

;; 3. (3/4)x^5 - 2x
(define ejemplo-racional
  (poli (nombre-var 'x)
        (mas-terminos (termino (coef-rac 3 4) (expo-nat 5))
          (mas-terminos (termino (coef-ent -2) (expo-nat 1))
            (sin-terminos)))))
;; (coef-rac? (termino->coef
;;   (mas-terminos->term (poli->terms ejemplo-racional))))     => #t
;; (coef-rac->num (termino->coef
;;   (mas-terminos->term (poli->terms ejemplo-racional))))     => 3
;; (coef-rac->den (termino->coef
;;   (mas-terminos->term (poli->terms ejemplo-racional))))     => 4

;; 4. 4x^5 - (3/2)x^2 + 7
(define ejemplo-tres-terminos
  (poli (nombre-var 'x)
        (mas-terminos (termino (coef-ent 4) (expo-nat 5))
          (mas-terminos (termino (coef-rac -3 2) (expo-nat 2))
            (mas-terminos (termino (coef-ent 7) (expo-nat 0))
              (sin-terminos))))))
;; (polinomio->pares ejemplo-tres-terminos)          => ((4 . 5) (-3/2 . 2) (7 . 0))
;; (termino? (mas-terminos->term
;;   (mas-terminos->resto (poli->terms ejemplo-tres-terminos)))) => #t

;; 5. El mismo polinomio del ejemplo 4, pero en la variable y.
(define ejemplo-en-y
  (poli (nombre-var 'y)
        (poli->terms ejemplo-tres-terminos)))
;; (nombre-var->s (poli->var ejemplo-en-y))          => y
;; (polinomio->pares ejemplo-en-y)                   => ((4 . 5) (-3/2 . 2) (7 . 0))

;; ---- Ejemplos de polinomio-cero ----

;; 1. Polinomio nulo en x.
(define cero-x (polinomio-cero 'x))
;; (polinomio->pares cero-x)                         => ()
;; (nombre-var->s (poli->var cero-x))                => x

;; 2. Polinomio nulo en y.
(define cero-y (polinomio-cero 'y))
;; (nombre-var->s (poli->var cero-y))                => y

;; 3. No tiene términos.
;; (poli? cero-x)                                    => #t
;; (sin-terminos? (poli->terms cero-x))              => #t

;; 4. Error: la variable debe ser un símbolo.
;; (polinomio-cero "x")   => error: La variable debe ser un simbolo
