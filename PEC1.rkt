#lang racket

(define lado 9)


(define test1
   '((5 0 0 0 0 0 0 0 0)
     (0 2 8 4 0 0 5 0 3)
     (1 0 0 2 7 0 0 0 6)
     (0 0 3 0 5 2 1 9 0)
     (7 0 6 0 1 0 2 0 8)
     (0 1 9 7 4 0 3 0 0)
     (6 0 0 0 9 4 0 0 2)
     (8 0 1 0 0 6 7 5 0)
     (0 0 0 0 0 0 0 0 4)))

(define test2
   '((5 6 0 0 0 4 1 0 0)
     (0 7 0 6 0 0 0 3 0)
     (0 0 0 1 2 0 0 6 0)
     (0 0 0 3 0 8 6 9 2)
     (0 8 0 0 7 0 0 4 0)
     (4 3 2 9 0 6 0 0 0)
     (0 5 0 0 9 7 0 0 0)
     (0 2 0 0 0 1 0 5 0)
     (0 0 9 5 0 0 0 8 6)))

(define test3
   '((0 0 4 0 3 0 0 6 0)
     (1 6 0 0 0 2 0 3 0)
     (9 0 2 8 0 6 4 5 0)
     (0 0 0 0 6 0 9 1 0)
     (0 0 0 0 0 0 0 0 0)
     (0 1 8 0 5 0 0 0 0)
     (0 2 6 1 0 5 3 0 4)
     (0 0 7 6 0 0 0 8 2)
     (0 4 0 0 2 0 6 0 0)))

(define testRandom
   '((0 9 0 3 5 0 0 0 0)
     (0 0 5 7 0 0 0 0 3)
     (1 3 2 0 0 0 0 0 0)
     (0 8 0 0 9 0 0 0 0)
     (0 7 0 5 0 6 0 3 0)
     (0 0 0 0 1 0 0 8 0)
     (0 0 0 0 0 0 1 2 6)
     (4 0 0 0 0 5 7 0 0)
     (0 0 0 0 7 9 0 4 0)))

(define test17
   '((3 0 8 0 0 0 0 0 0)
     (0 0 0 0 4 0 5 0 7)
     (6 7 0 0 5 8 0 1 9)
     (1 8 0 0 0 0 0 0 0)
     (7 9 0 8 2 3 0 4 5)
     (0 0 0 0 0 0 0 9 3)
     (4 6 0 7 8 0 0 3 1)
     (2 0 9 0 1 0 0 0 0)
     (0 0 0 0 0 0 4 0 6)))

(define test20 
   '((8 0 0 4 6 0 0 0 0)
     (0 0 0 0 1 0 0 0 9)
     (0 7 0 3 0 8 6 0 0)
     (2 0 0 0 0 0 0 9 0)
     (3 8 0 0 7 0 0 2 1)
     (0 5 0 0 0 0 0 0 8)
     (0 0 4 5 0 6 0 1 0)
     (5 0 0 4 0 0 0 0 0)
     (0 0 0 0 9 3 0 0 6)))


;imprime el estado de un sudoku por pantalla
(define (imprimirSudoku sudoku)  
  (for* ([i lado]
         [j lado])
    (when(and (equal? (modulo i 3) 0)(= j 0))
      (printf "+---------+---------+---------+\n"))
      (when (equal? (modulo j 3) 0)
        (printf "~a" "|"))
      (printf " ")
      (printf "~a" (list-ref (list-ref sudoku i) j))
      (printf " ")
      (when (= j 8)(printf "~a" "|\n"))
      (when (and (= i 8) (= j 8))
        (printf "+---------+---------+---------+\n"))))

;comprueba si una determinada casilla está vacia (contiene 0) o no
(define (checkPosicionVacia sudoku fila columna)
  (if(= (list-ref(list-ref sudoku fila) columna) 0) #t #f)
)

;Se pasa la fila mediante el procedimiento (list-ref test1 i) i, true si se repite valores
(define (comprobarLista lst) 
  (if(check-duplicates lst) #t #f)
  )
;devuelve true si el elemento ya existe en la lista, y false si aun no se encuentra
(define (comprobarDuplicado lst valor) 
  (for/or ([i (in-range lado)])
    (= valor (list-ref lst i))
   )
  )

;Comprueba si en la columna seleccionada existe ya un valor determinado
(define (comprobarColumna sudoku columna valor)
     (for/or ([fila lado])
         (= valor (list-ref (list-ref sudoku fila)columna))
       )
  )

;Procedimiento que encadena un elemento a una lista
(define (append-elt lst x)
  (append lst (list x)))

;Comprueba si el cuadrante admite o no un numero determinado en alguna posición
(define (comprobarCuadrante sudoku cuadrante valor)
  (define n (modulo cuadrante 3))
  (define aux '())
  ;(set n (modulo 3 n))
  (cond
    [(and (<= cuadrante 2) (>= cuadrante 0))
     (for* ([i (in-range (* 3 n) (+ 3 (* 3 n)))] [j (in-range 0 3)])
      ;(printf "~a~a " i j)
      ;(printf "t1")
      (set! aux (append-elt aux (list-ref (list-ref sudoku j) i)))
     )]
    [(and (<= cuadrante 5) (>= cuadrante 3))
     (for* ([i (in-range (* 3 n) (+ 3 (* 3 n)))] [j (in-range 3 6)])
      ;(printf "t2")
      ;(printf "~a~a " i j)
      (set! aux (append-elt aux (list-ref (list-ref sudoku j) i)))
     )]
    [(and (<= cuadrante 8) (>= cuadrante 6))
      (for* ([i (in-range (* 3 n) (+ 3 (* 3 n)))] [j (in-range 6 9)])
      ;(printf "~a~a " i j)
      (set! aux (append-elt aux (list-ref (list-ref sudoku j) i)))
      
    )]
    [else (printf "rompe")])
  
  (if (comprobarDuplicado aux valor) #t #f)
  )

;Comprueba que numeros son validos para una determinada casilla
(define (getCaminos sudoku fila columna n)
  (if (not(= n 10))            
    (if (and (not (comprobarDuplicado (list-ref sudoku fila) n)) (not (comprobarColumna sudoku columna n)) (not (comprobarCuadrante sudoku (getCuadrante fila columna) n)))   ; outer then-expr (and inner if-expr)
        (append-elt (getCaminos sudoku fila columna (+ 1 n)) n)                
        (getCaminos sudoku fila columna (+ 1 n)))
    '())
)

;Sustituye el valor de una casilla determinada por otro
(define (sustituir sudoku fila col valor)
  (list-set sudoku fila (list-set (list-ref sudoku fila) col valor))
  )

;Devuelve el cuadrante en el que una casilla está ubicada
(define (getCuadrante fila columna)
  (cond
    [(and (>= fila 0) (<= fila 2) (>= columna 0) (<= columna 2)) 0]
    [(and (>= fila 0) (<= fila 2) (>= columna 3) (<= columna 5)) 1]
    [(and (>= fila 0) (<= fila 2) (>= columna 6) (<= columna 8)) 2]
    [(and (>= fila 3) (<= fila 5) (>= columna 0) (<= columna 2)) 3]
    [(and (>= fila 3) (<= fila 5) (>= columna 3) (<= columna 5)) 4]
    [(and (>= fila 3) (<= fila 5) (>= columna 6) (<= columna 8)) 5]
    [(and (>= fila 6) (<= fila 8) (>= columna 0) (<= columna 2)) 6]
    [(and (>= fila 6) (<= fila 8) (>= columna 3) (<= columna 5)) 7]
    [(and (>= fila 6) (<= fila 8) (>= columna 6) (<= columna 8)) 8]
    )
)

;Procedimiento que realiza una busqueda en profundidad
(define (busqProfundidad fila columna sudoku)
    (imprimirSudoku sudoku)
    (if (> fila 8) #t
       (if (> columna 8)
           (busqProfundidad (+ 1 fila) 0 sudoku)
           (if (equal? (checkPosicionVacia sudoku fila columna ) #f)
                (busqProfundidad fila (+ 1 columna) sudoku)
                (if (equal? 0 (length (getCaminos sudoku fila columna 0)))#f
                    (for/or ([i (length (getCaminos sudoku fila columna 0))])
                      (busqProfundidad fila (+ 1 columna) (sustituir sudoku fila columna (list-ref (getCaminos sudoku fila columna 0) i)))
                      )
                      )
                )
        )
     )
 )

;Procedimiento que realiza una busqueda en anchura
(define (busqAnchura fila columna sudoku)
    (imprimirSudoku sudoku)
    (if (> fila 8) #t
       (if (> columna 8)
           (busqAnchura (+ 1 fila) 0 sudoku)
           (if (equal? (checkPosicionVacia sudoku fila columna ) #f)
                (busqAnchura fila (+ 1 columna) sudoku)
                (if (equal? 0 (length (getCaminos sudoku fila columna 0)))#f
                    (for/or ([i (length (getCaminos sudoku fila columna 0))])
                      (busqAnchura fila (+ 1 columna) (sustituir sudoku fila columna (list-ref (reverse(getCaminos sudoku fila columna 0)) i)))
                      )
                      )
                )
        )
     )
 )

;Procedimiento que comienza la resolución del sudoku 0 para Anchura y 1 para Profundidad
(define (comenzarResolucion sudoku metodo)
  (cond
    [(equal? metodo 0) (busqAnchura 0 0 sudoku)]
    [(equal? metodo 1) (busqProfundidad 0 0 sudoku)]
    )
)
  


