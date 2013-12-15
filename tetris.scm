(declare (block) (fixnum) (usual-integrations))

(use ncurses srfi-25)

;;For use when compiled
;;(import tetromino)

(declare (uses tetromino))
(load "tetromino.scm")

;; Tetris in scheme

(define STARTX 0)
(define STARTY 0)
(define ENDX 79)
(define ENDY 24)

(define CELL_CHAR #\#)
(define TIME_OUT 300)

(define cols #f)
(define lines #f)

(define color car)
(define display? cdr)
(define color-set! set-car!)
(define display-set! set-cdr!)

;;Function to display to the ncurses window the game play area.
(define (tetra-display win area)
  (wclear win)
  (do ([i STARTX (add1 i)])
    ((>= i ENDX))
    (do ([j STARTY (add1 j)])
      ((>= j ENDY))
      (let ([cell (array-ref area i j)])
        (unless (not (display? cell))
          (wattron win (COLOR_PAIR (color cell)))
          (mvwaddch win j i CELL_CHAR)
          (wattroff win (COLOR_PAIR (color cell)))))))
  (wrefresh win))

;; Check if an item is within boundary limits
(define (out-limits? coord)
  (or (>= (car coord) ENDX)
      (>= (cdr coord) ENDY)
      (< (car coord) STARTX)
      (< (cdr coord) STARTY)))

;; If disp is set to true, this function sets the array where the block
;; would be to be displayable. If false, erase the last position of the block
;; from the grid until we get a change in state
(define (update-state tetra disp area)
  (map (lambda (coord)
         (unless (out-limits? coord)
           (let ([cell (array-ref area (car coord) (cdr coord))])
             (display-set! cell disp)
             (color-set! cell (caar tetra)))))
       (calc-offset tetra)))

;; Check if the block needs to stop moving, from either touching the ground
;; or from touching another block
(define (check-condition tetra area)
  (let item ([coordlist (calc-offset tetra)])
    (cond ((null? coordlist) #t)    ; All coordinates are within the grid
          ((out-limits? (car coordlist)) #f)
          ((display? (array-ref area (caar coordlist) (cdar coordlist))) #f)
          (else
            (item (cdr coordlist))))))

;; Handle the inputs. Returns a new block based on input. If we quit, we will
;; return a boolean instead.
(define (input block) 
  (case (getch)   ; Get user input
    ((#\q) #f)
    ((#\w) (move-block block 0 -1))
    ((#\s) (move-block block 0 1))
    ((#\a) (move-block block -1 0))
    ((#\d) (move-block block 1 0))
    ((#\c) (new-tetra))
    ((#\space)  (rot-cw block))
    (else block)))

;; The entry into the game. Set up the terminal and out playing grid, and 
;; recurse until an exit status occurs.
(define (main)
  ;;Settings for the terminal session
  (initscr)     ; Initialize the screen
  (start_color) ; Start the terminal in color mode
  (noecho)
  (cbreak)
  (curs_set 0)
  (keypad (stdscr) #t)
  (timeout TIME_OUT)

  ;;Settings for the window
  (set! cols (COLS))
  (set! lines (LINES))
  (set! ENDX (sub1 cols))
  (set! ENDY (sub1 lines))

  ;;Iterate through colors to create default color pairs
  (let set_colors ([color COLOR_RED])
    (unless (eqv? color (add1 COLOR_WHITE))
      (init_pair color color COLOR_BLACK)
      (set_colors (add1 color))))

  ;;Start setting up the work area
  (let ([workarea (make-array (shape 0 cols 0 lines))])
    (do ([i 0 (add1 i)])
      ((>= i cols))
      (do([j 0 (add1 j)])
        ((>= j lines))
        (array-set! workarea i j (cons 0 #f))))
        (tetra-display (stdscr) workarea)

        ;;Start the main game loop
        (let loop ([continue #t] [block (L-block)])
          (unless (not continue)
            ;; Remove the position of the block from the grid until we update it
            (update-state block #f workarea)
            
            ;; Update the block state based on input
            (let ((result (input block)))
              (if (boolean? result) 
                (set! continue result)  ; Exit out of our game loop     
                (set! block result)))   ; Otherwise update our block
            
            (set! block (move-block block 0 1))   ; Move our block anyways
            
            ;; Update the state of the grid now
            (update-state block #t workarea)
            (tetra-display (stdscr) workarea)
            (loop continue block))))    ; Keep looping
  (echo)
  (nocbreak)
  (curs_set 1)
  (endwin))

(main)
