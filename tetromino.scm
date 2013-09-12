;;Defintions and functions regarding tetrominoes

;;Create the basic tetromino block
(define (make-tetrimino x1 y1 x2 y2 x3 y3 x4 y4)
  (list (cons x1 y1) (cons x2 y2) (cons x3 y3) (cons x4 y4)))

(define (L-block)
  (make-tetrimino 0 0 1 0 2 0 3 0))

(define (J-block)
  (make-tetrimino 0 0 0 1 1 1 2 1))

(define (L-block)
  (make-tetrimino 0 1 1 1 2 1 2 0))

(define (O-block)
  (make-tetrimino 0 0 0 1 1 0 1 1))

(define (S-block)
  (make-tetrimino 1 0 2 0 0 1 1 1))

(define (T-block)
  (make-tetrimino 1 0 0 1 1 1 2 1))

(define (Z-block)
  (make-tetrimino 0 0 1 0 1 1 2 1))


;;Functions that work on blocks

;; Rotates a block clockwise
(define (rot-clockwise tetra)
  (map 
    (lambda (pair)
      (cons (* (cdr pair) -1) (car pair)))
    tetra))

;;Rotates a block counterclockwise
(define (rot-counterclockwise tetra)
  (map
    (lambda (pair)
      (cons (cdr pair) (* (car pair) -1)))
    tetra))

(define (move tetra x y)
  (map (lambda (coord)
         (cons (+ (car coord) x) (+ (cdr coord) y)))
       tetra))
