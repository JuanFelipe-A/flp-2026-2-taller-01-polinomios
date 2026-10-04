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
 polinomio-cero insertar-termino coeficiente-de eliminar-termino)

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

;; --- Interfaz del TAD (pendiente) ---

(define polinomio-cero
  (lambda (variable)
    (eopl:error 'polinomio-cero "Sin implementar")))

(define insertar-termino
  (lambda (polinomio coeficiente exponente)
    (eopl:error 'insertar-termino "Sin implementar")))

(define coeficiente-de
  (lambda (polinomio exponente)
    (eopl:error 'coeficiente-de "Sin implementar")))

(define eliminar-termino
  (lambda (polinomio exponente)
    (eopl:error 'eliminar-termino "Sin implementar")))
