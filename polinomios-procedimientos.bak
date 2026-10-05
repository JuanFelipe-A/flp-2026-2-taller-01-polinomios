#lang eopl
;Autores: Juan Felipe Aristizabal 2459364-3743, Juan Sebastian Huertas 2459505-3743

;; Taller 1 — Polinomios dispersos.
;; Parte 2: representación basada en procedimientos.
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

;; Representación con procedimientos: cada dato es un procedimiento que recibe
;; un mensaje y responde. El mensaje 'etiqueta devuelve el nombre de la variante
;; y los demás mensajes devuelven los campos. Desde afuera no se ve qué hay
;; adentro: solo se puede preguntar.

;; es-variante? : any x symbol -> boolean
;; Dice si x es un dato de la variante con esa etiqueta.
(define es-variante?
  (lambda (x etiqueta)
    (and (procedure? x)
         (eq? (x 'etiqueta) etiqueta))))

;; extraer : symbol x (any -> boolean) x symbol x symbol x any -> any
;; Le pide el campo al dato. Si el dato no es de la variante esperada, da error.
(define extraer
  (lambda (quien variante? variante campo dato)
    (if (variante? dato)
        (dato campo)
        (eopl:error quien "Se esperaba un dato de la variante ~s y se recibio ~s"
                    variante dato))))

;; --- Constructores ---

;; poli : nombre-var x terminos -> polinomio
;; Arma un polinomio con su variable y sus términos.
(define poli
  (lambda (var terms)
    (lambda (mensaje)
      (cond ((eq? mensaje 'etiqueta) 'poli)
            ((eq? mensaje 'var) var)
            ((eq? mensaje 'terms) terms)
            (else (eopl:error 'poli "Mensaje desconocido: ~s" mensaje))))))

;; nombre-var : symbol -> nombre-var
;; Arma la variable del polinomio.
(define nombre-var
  (lambda (s)
    (lambda (mensaje)
      (cond ((eq? mensaje 'etiqueta) 'nombre-var)
            ((eq? mensaje 's) s)
            (else (eopl:error 'nombre-var "Mensaje desconocido: ~s" mensaje))))))

;; sin-terminos : () -> terminos
;; Arma la lista de términos vacía.
(define sin-terminos
  (lambda ()
    (lambda (mensaje)
      (cond ((eq? mensaje 'etiqueta) 'sin-terminos)
            (else (eopl:error 'sin-terminos "Mensaje desconocido: ~s" mensaje))))))

;; mas-terminos : termino x terminos -> terminos
;; Arma una lista con term al frente y el resto detrás.
(define mas-terminos
  (lambda (term resto)
    (lambda (mensaje)
      (cond ((eq? mensaje 'etiqueta) 'mas-terminos)
            ((eq? mensaje 'term) term)
            ((eq? mensaje 'resto) resto)
            (else (eopl:error 'mas-terminos "Mensaje desconocido: ~s" mensaje))))))

;; termino : coeficiente x exponente -> termino
;; Arma un término.
(define termino
  (lambda (coef expo)
    (lambda (mensaje)
      (cond ((eq? mensaje 'etiqueta) 'termino)
            ((eq? mensaje 'coef) coef)
            ((eq? mensaje 'expo) expo)
            (else (eopl:error 'termino "Mensaje desconocido: ~s" mensaje))))))

;; coef-ent : int -> coeficiente
;; Arma un coeficiente entero.
(define coef-ent
  (lambda (n)
    (lambda (mensaje)
      (cond ((eq? mensaje 'etiqueta) 'coef-ent)
            ((eq? mensaje 'n) n)
            (else (eopl:error 'coef-ent "Mensaje desconocido: ~s" mensaje))))))

;; coef-rac : int x int -> coeficiente
;; Arma un coeficiente racional (fracción).
(define coef-rac
  (lambda (num den)
    (lambda (mensaje)
      (cond ((eq? mensaje 'etiqueta) 'coef-rac)
            ((eq? mensaje 'num) num)
            ((eq? mensaje 'den) den)
            (else (eopl:error 'coef-rac "Mensaje desconocido: ~s" mensaje))))))

;; expo-nat : int -> exponente
;; Arma un exponente.
(define expo-nat
  (lambda (k)
    (lambda (mensaje)
      (cond ((eq? mensaje 'etiqueta) 'expo-nat)
            ((eq? mensaje 'k) k)
            (else (eopl:error 'expo-nat "Mensaje desconocido: ~s" mensaje))))))

;; --- Predicados ---

;; poli? : any -> boolean
;; Dice si x fue construido con poli.
(define poli?
  (lambda (x) (es-variante? x 'poli)))

;; nombre-var? : any -> boolean
;; Dice si x fue construido con nombre-var.
(define nombre-var?
  (lambda (x) (es-variante? x 'nombre-var)))

;; sin-terminos? : any -> boolean
;; Dice si x fue construido con sin-terminos.
(define sin-terminos?
  (lambda (x) (es-variante? x 'sin-terminos)))

;; mas-terminos? : any -> boolean
;; Dice si x fue construido con mas-terminos.
(define mas-terminos?
  (lambda (x) (es-variante? x 'mas-terminos)))

;; termino? : any -> boolean
;; Dice si x fue construido con termino.
(define termino?
  (lambda (x) (es-variante? x 'termino)))

;; coef-ent? : any -> boolean
;; Dice si x fue construido con coef-ent.
(define coef-ent?
  (lambda (x) (es-variante? x 'coef-ent)))

;; coef-rac? : any -> boolean
;; Dice si x fue construido con coef-rac.
(define coef-rac?
  (lambda (x) (es-variante? x 'coef-rac)))

;; expo-nat? : any -> boolean
;; Dice si x fue construido con expo-nat.
(define expo-nat?
  (lambda (x) (es-variante? x 'expo-nat)))

;; --- Extractores ---

;; poli->var : polinomio -> nombre-var
;; Devuelve la variable del polinomio.
(define poli->var
  (lambda (p) (extraer 'poli->var poli? 'poli 'var p)))

;; poli->terms : polinomio -> terminos
;; Devuelve la lista de términos del polinomio.
(define poli->terms
  (lambda (p) (extraer 'poli->terms poli? 'poli 'terms p)))

;; nombre-var->s : nombre-var -> symbol
;; Devuelve el símbolo de la variable.
(define nombre-var->s
  (lambda (v) (extraer 'nombre-var->s nombre-var? 'nombre-var 's v)))

;; mas-terminos->term : terminos -> termino
;; Devuelve el primer término de una lista no vacía.
(define mas-terminos->term
  (lambda (ts) (extraer 'mas-terminos->term mas-terminos? 'mas-terminos 'term ts)))

;; mas-terminos->resto : terminos -> terminos
;; Devuelve la lista sin su primer término.
(define mas-terminos->resto
  (lambda (ts) (extraer 'mas-terminos->resto mas-terminos? 'mas-terminos 'resto ts)))

;; termino->coef : termino -> coeficiente
;; Devuelve el coeficiente del término.
(define termino->coef
  (lambda (t) (extraer 'termino->coef termino? 'termino 'coef t)))

;; termino->expo : termino -> exponente
;; Devuelve el exponente del término.
(define termino->expo
  (lambda (t) (extraer 'termino->expo termino? 'termino 'expo t)))

;; coef-ent->n : coeficiente -> int
;; Devuelve el entero de un coef-ent.
(define coef-ent->n
  (lambda (c) (extraer 'coef-ent->n coef-ent? 'coef-ent 'n c)))

;; coef-rac->num : coeficiente -> int
;; Devuelve el numerador de un coef-rac.
(define coef-rac->num
  (lambda (c) (extraer 'coef-rac->num coef-rac? 'coef-rac 'num c)))

;; coef-rac->den : coeficiente -> int
;; Devuelve el denominador de un coef-rac.
(define coef-rac->den
  (lambda (c) (extraer 'coef-rac->den coef-rac? 'coef-rac 'den c)))

;; expo-nat->k : exponente -> int
;; Devuelve el entero de un expo-nat.
(define expo-nat->k
  (lambda (e) (extraer 'expo-nat->k expo-nat? 'expo-nat 'k e)))

;; ---- Interfaz: inicio (igual en listas y procedimientos) ----
;; Aquí solo se usan constructores, predicados y extractores (nada de car ni cdr).

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
    (if (coef-ent? c)
        (coef-ent->n c)
        (/ (coef-rac->num c) (coef-rac->den c)))))

;; crear-termino : numero x natural -> termino
;; Arma un término con un coeficiente y un exponente de Racket.
(define crear-termino
  (lambda (coeficiente exponente)
    (termino (concreto->coeficiente coeficiente) (expo-nat exponente))))

;; polinomio-cero : symbol -> polinomio
;; Devuelve el polinomio 0 en esa variable. Da error si no es un símbolo.
(define polinomio-cero
  (lambda (variable)
    (if (symbol? variable)
        (poli (nombre-var variable) (sin-terminos))
        (eopl:error 'polinomio-cero "La variable debe ser un simbolo"))))

;; insertar-en-terminos : terminos x numero x natural -> terminos
;; Recorre la lista una sola vez. Si el exponente actual es mayor, sigue con el
;; resto; si es menor, el término nuevo va antes; si es igual, suma los
;; coeficientes (si da 0 quita el término).
(define insertar-en-terminos
  (lambda (terminos coeficiente exponente)
    (if (sin-terminos? terminos)
        (mas-terminos (crear-termino coeficiente exponente) terminos)
        (let* ((actual (mas-terminos->term terminos))
               (resto  (mas-terminos->resto terminos))
               (expo   (expo-nat->k (termino->expo actual))))
          (cond
            ((> expo exponente)
             (mas-terminos actual (insertar-en-terminos resto coeficiente exponente)))
            ((< expo exponente)
             (mas-terminos (crear-termino coeficiente exponente) terminos))
            (else
             (let ((suma (+ (coeficiente->concreto (termino->coef actual)) coeficiente)))
               (if (zero? suma)
                   resto
                   (mas-terminos (crear-termino suma exponente) resto)))))))))

;; insertar-termino : polinomio x numero x natural -> polinomio
;; Inserta el término en su lugar. Si el exponente ya está, suma los
;; coeficientes (si da 0 quita el término). Con coeficiente 0 no cambia nada.
;; Da error si el exponente no es un entero >= 0 o el coeficiente no es exacto.
(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (cond
      ((not (exponente-concreto? exponente))
       (eopl:error 'insertar-termino "El exponente debe ser un entero no negativo"))
      ((not (coeficiente-concreto? coeficiente))
       (eopl:error 'insertar-termino "El coeficiente debe ser un numero exacto"))
      ((not (poli? polinomio))
       (eopl:error 'insertar-termino "El primer argumento debe ser un polinomio"))
      ((zero? coeficiente)
       polinomio)
      (else
       (poli (poli->var polinomio)
             (insertar-en-terminos (poli->terms polinomio) coeficiente exponente))))))

;; coeficiente-de : polinomio x natural -> numero
;; Devuelve el coeficiente del término con ese exponente. Da error si no existe.
(define coeficiente-de
  (lambda (polinomio exponente)
    (eopl:error 'coeficiente-de "Sin implementar")))

;; eliminar-termino : polinomio x natural -> polinomio
;; Devuelve el polinomio sin el término con ese exponente. Da error si no existe.
(define eliminar-termino
  (lambda (polinomio exponente)
    (eopl:error 'eliminar-termino "Sin implementar")))

;; polinomio->pares : polinomio -> lista de pares (coeficiente . exponente)
;; Muestra el polinomio como lista de pares, de mayor a menor exponente.
;; Sirve para ver los resultados; no es parte de la interfaz.
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

;; ---- Ejemplos de construcción ----

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

;; 4. Sobre el nulo se puede insertar un término.
(define cero-mas-siete (insertar-termino (polinomio-cero 'z) 7 0))
;; (polinomio->pares cero-mas-siete)                 => ((7 . 0))

;; 5. Error: la variable debe ser un símbolo.
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

;; 1. Insertar en el polinomio nulo.
;; (polinomio->pares (insertar-termino (polinomio-cero 'x) 7 0))   => ((7 . 0))

;; 2. Exponente nuevo, queda en el medio.
(define p-con-x3 (insertar-termino p 2 3))
;; (polinomio->pares p-con-x3)                       => ((4 . 5) (2 . 3) (-3/2 . 2) (7 . 0))

;; 3. Exponente nuevo y mayor que todos, queda de primero.
(define p-con-x9 (insertar-termino p 1 9))
;; (polinomio->pares p-con-x9)                       => ((1 . 9) (4 . 5) (-3/2 . 2) (7 . 0))

;; 4. Exponente nuevo y menor que todos, queda de último.
(define q-al-final
  (insertar-termino (insertar-termino (polinomio-cero 'x) 4 5) 9 3))
;; (polinomio->pares q-al-final)                     => ((4 . 5) (9 . 3))

;; 5. El exponente ya existe: se suman los coeficientes.
(define p-suma (insertar-termino p 1 2))
;; (polinomio->pares p-suma)                         => ((4 . 5) (-1/2 . 2) (7 . 0))

;; 6. La suma da cero: el término desaparece.
(define p-cancelado (insertar-termino p 3/2 2))
;; (polinomio->pares p-cancelado)                    => ((4 . 5) (7 . 0))

;; 7. Coeficiente 0: el polinomio no cambia.
;; (polinomio->pares (insertar-termino p 0 2))       => ((4 . 5) (-3/2 . 2) (7 . 0))
;; (polinomio->pares (insertar-termino p 0 8))       => ((4 . 5) (-3/2 . 2) (7 . 0))

;; 8. Error: exponente negativo.
;; (insertar-termino p 5 -1)  => error: El exponente debe ser un entero no negativo

;; 9. Error: coeficiente no exacto.
;; (insertar-termino p 1.5 2) => error: El coeficiente debe ser un numero exacto

;; Pendiente (compañero): 5 ejemplos de coeficiente-de y 5 de eliminar-termino,
;; cada grupo con su caso de error.
