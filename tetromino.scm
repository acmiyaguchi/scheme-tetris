;;Tetromino library
(declare (unit tetromino))

(module tetromino *
        (import scheme chicken)
        (use ncurses))

(use ncurses extras)

;; The tetromino block is a defined as a list structure
;; with the first half describing the current position on the grid 
;; and the color to display it as. The second half contains a list 
;; of coordinates that define the shape. 
;;
;; (COLOR, OFFSET-PAIR) . (P1, P2, P3, P4)
;;
;; We can access the individual elements using the following definitions

(define block-color caar)   ;;First element of first pair
(define block-offset cdar)  ;;Second element of first pair
(define block-coords cadr)  ;;First element of second pair

;; We should also define the max blocks

(define MAX_TETRA 7)

;; All coordinate pairs are in the positive xy plane, which starts 
;; from the upper left and goes to the bottom right of the screen.
 
;; A template for creating the basic building tetromino block
;; TODO: Change the offset for a real game
;;
(define (make-tetromino color x1 y1 x2 y2 x3 y3 x4 y4)
  (list (cons color (cons 0 0)) ;;associated metadata
        (list (cons x1 y1) 
              (cons x2 y2) 
              (cons x3 y3)
              (cons x4 y4))))

;; Listed below are all the possible configurations of blocks
;; and their approximate look in ascii

;;
;; ## ## ## ##
;; 
(define (I-block)
  (make-tetromino 
    COLOR_RED 0 0 1 0 2 0 3 0))

;;
;; ##
;; ## ## ##
;;
(define (J-block)
  (make-tetromino 
    COLOR_GREEN 0 0 0 1 1 1 2 1))

;;       ##
;; ## ## ##
;;
(define (L-block)
  (make-tetromino 
    COLOR_YELLOW 0 1 1 1 2 1 2 0))

;;
;; ## ##
;; ## ##
;;
(define (O-block)
  (make-tetromino 
    COLOR_BLUE 0 0 0 1 1 0 1 1))

;;
;;    ## ##
;; ## ##
;;
(define (S-block)
  (make-tetromino 
    COLOR_MAGENTA 
    1 0 2 0 0 1 1 1))

;;
;;    ##
;; ## ## ##
;;
(define (T-block)
  (make-tetromino 
    COLOR_CYAN 1 0 0 1 1 1 2 1))

;;
;; ## ##
;;    ## ##
;;
(define (Z-block)
  (make-tetromino 
    COLOR_WHITE 0 0 1 0 1 1 2 1))


;; Below are functions that work on the tetromino block datatype

;; Generate a random tetromino
(define (new-tetra)
  (case (random MAX_TETRA)
    ((0) (I-block))
    ((1) (J-block))
    ((2) (L-block))
    ((3) (O-block))
    ((4) (S-block))
    ((5) (T-block))
    ((6) (Z-block))))

;; Return a list of the actual coordinates of the blocks. The offset is applied to
;; each coordinate base to get the relative position of the block to the origin. 
;;
(define (calc-offset tetra)
  (let ([off (block-offset tetra)])
    (map (lambda (item) (cons (+ (car item) (car off))
                              (+ (cdr item) (cdr off))))
         (block-coords tetra))))

;;Displace the block by displacing the current offset by a delta x
;;and a delta y. Returns a tetra block. 
;;
(define (move-block tetra dx dy)
  (list (cons (block-color tetra)
              (cons (+ (cadar tetra) dx)
                    (+ (cddar tetra) dy)))
        (block-coords tetra)))

;; Rotate a block clockwise. We transform the block based on its position
;; from a relative origin. Takes a basic tetra block and returns a modified
;; block with transformed coordinates. 
;;
;; TODO: Rotate the block around a central axis rather than corner point
;;
(define (rot-cw tetra)
  (list (car tetra)
        (map (lambda 
               (pair)(cons (* (cdr pair) -1) (car pair)))
             (block-coords tetra))))

;;Same as rot-cw, except rotation is done counter-clockwise. The transform is 
;;based on the position from a relative origin to the coordinates. Takes a tetra
;;block and returns a modified block with transformed coordinates.
;;
;;TODO: See above
;;
(define (rot-ccw tetra)
  (list (car tetra)
        (map(lambda (pair)
              (cons (cdr pair) (* (car pair) -1)))
          (block-coords tetra))))

;; Quick tests

(let ((x (L-block)))
  (display x)
  (display (rot-cw x)))
