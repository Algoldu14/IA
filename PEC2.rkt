#lang racket
(define partida1 '(('libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre) 
                   ('libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre)
                       ('libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre)
                       ('libre 'libre 'libre 'blanc 'negra 'libre 'libre 'libre)
                       ('libre 'libre 'libre 'negra 'blanc 'libre 'libre 'libre)
                       ('libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre)
                       ('libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre)
                       ('libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre)))

;metodo que comprueba si un movimiento cambia una ficha en una lista auxiliar
(define (compruebaLista pos lista color)
  #t
)

;metodo que comprueba para izquiera
(define (compruebaIzq pos lista color)
  (define contador 0)
  (define aux 0)
  (for/and ([i lista])
    (equal? (getColorOpuesto color) (list-ref lista i))
      (set! contador (+ contador 1))
    )
  
  contador
  )

;metodo que genera una lista auxiliar a partir de un numero de fila
(define (generaFila tablero fila)
  (list-ref tablero fila)
  )

;metodo que genera una lista auxiliar a partir de un numero de columna
(define (generarColumna tablero  columna )
  (define aux '())
  (for ([i 7]) (set! aux (append-elt aux (list-ref (list-ref tablero i) columna))))
  aux
  )


(define (append-elt lst x)
  (append lst (list x)))

;metodo que retorna el color opuesto
(define (getColorOpuesto color)
  (if (equal? color 'blanc) 'negra 'blanc)
)

;metodo que cuenta la cantidad de casillas libres
(define (cuentaLibres tablero)
  (length (indexes-of (flatten tablero) 'libre))
 )

;metodo que cuenta la cantidad de casillas negras
(define (cuentaNegras tablero)
  (length (indexes-of (flatten tablero) 'blanc))
 )

;metodo que cuenta la cantidad de casillas blancas
(define (cuentaBlancas tablero)
  (length (indexes-of (flatten tablero) 'negra))
 )