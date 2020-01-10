#lang racket
(define partida1 (list 'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                       'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
                       'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                       'libre 'libre 'libre 'blanc 'negra 'libre 'libre 'libre 
                       'libre 'libre 'libre 'negra 'blanc 'libre 'libre 'libre  
                       'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                       'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
                       'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre))

(define (getValor tablero fila columna)
  (list-ref(list-ref tablero fila) columna)
  )

;metodo que comprueba si un movimiento cambia una ficha en vertical
(define (cambia?vertical tablero posicion color)
  (cond
    [(< posicion 15) 0]
    [(> posicion 47) 0]
    [else
     (cond
      [(< posicion 15) #t]
      [(> posicion 47) #t]
      [else #f])
     ])
  )

;metodo que cuenta la cantidad de casillas libres
(define (cuentaLibres tablero)
  (length (indexes-of tablero 'libre))
 )

;metodo que cuenta la cantidad de casillas negras
(define (cuentaNegras tablero)
  (length (indexes-of tablero 'blanc))
 )

;metodo que cuenta la cantidad de casillas blancas
(define (cuentaBlancas tablero)
  (length (indexes-of tablero 'negra))
 )