;;Defintions and functions regarding tetrominoes

(use ncurses)

;; caar -> color
;; cdar -> offset pair
;; cadr -> coords
(define block-color caar)
(define block-offset cdar)
(define block-coords cadr)

;;Create the basic tetromino block
(define (make-tetrimino color x1 y1 x2 y2 x3 y3 x4 y4)
  (list (cons color (cons 0 0)) ;;associated metadata
        (list (cons x1 y1) 
              (cons x2 y2) 
              (cons x3 y3)
              (cons x4 y4))))

(define (L-block)
  (make-tetrimino COLOR_RED 0 0 1 0 2 0 3 0))

(define (J-block)
  (make-tetrimino COLOR_GREEN 0 0 0 1 1 1 2 1))

(define (L-block)
  (make-tetrimino COLOR_YELLOW 0 1 1 1 2 1 2 0))

(define (O-block)
  (make-tetrimino COLOR_BLUE 0 0 0 1 1 0 1 1))

(define (S-block)
  (make-tetrimino COLOR_MAGENTA 1 0 2 0 0 1 1 1))

(define (T-block)
  (make-tetrimino COLOR_CYAN 1 0 0 1 1 1 2 1))

(define (Z-block)
  (make-tetrimino COLOR_WHITE 0 0 1 0 1 1 2 1))

;;-------------------------------
;;Functions that work on blocks
;;-------------------------------
;;Helper function of the rotate series of function
;;Takes in a block, and returns the coordinate list with subtracted offsets
(define (calc-offset off ls)
  (map (lambda (item)
         (+ (car item) (car off))
         (+ (cdr item) (cdr off)))
       ls))

;;Displace the block by modifying the current offsets
(define (move-block tetra dx dy)
    (set! (cadar tetra) (+ (cadar tetra) dx))
    (set! (cddar tetra) (+ (cddar tetra) dy)))

;; Rotates a block clockwise
(define (rot-cw tetra)
  (cons (car tetra)
        (map (lambda 
               (pair)(cons (* (cdr pair) -1) (car pair)))
             (block-coords tetra))))

;;Rotates a block counterclockwise
(define (rot-ccw tetra)
  (cons (car tetra)
        (map(lambda (pair)
              (cons (cdr pair) (* (car pair) -1)))
          (block-coords tetra))))
