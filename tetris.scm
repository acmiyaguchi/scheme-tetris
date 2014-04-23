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

(define ILLEGAL     0)
(define NEXT      1)
(define CONTINUE    2)

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
          (mvwaddch win j (* i 2) CELL_CHAR)
          (mvwaddch win j (add1 (* i 2)) CELL_CHAR )
          (wattroff win (COLOR_PAIR (color cell)))))))
  (box win 0 0)
  (wrefresh win))

;; Check if item is within X boundaries
(define (out-x? coord)
  (or (>= (car coord) ENDX)
      (< (car coord) STARTX)))

;; Check if item is within Y boundaries
(define (out-y? coord)
  (or (>= (cdr coord) ENDY)
      (< (cdr coord) STARTY)))

;; Check if an item is within boundary limits
(define (out-limits? coord)
  (or (out-x? coord)
      (out-y? coord)))

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
    (cond ((null? coordlist) CONTINUE)    ; All coordinates are within the grid
          ((out-x? (car coordlist)) ILLEGAL)
          ((out-y? (car coordlist)) NEXT)
          ((display? (array-ref area (caar coordlist) (cdar coordlist))) NEXT)
          (else
            (item (cdr coordlist))))))

;; Handle the inputs. Returns a new block based on input. If we quit, we will
;; return a boolean instead.
(define (input block) 
    (case (getch)
      ((#\q) #f)
      ((#\w) (move-block block 0 -1))
      ((#\s) (move-block block 0 1))
      ((#\a) (move-block block -1 0))
      ((#\d) (move-block block 1 0))
      ((#\c) (new-tetra))
      ((#\space)  (rot-cw block))
      (else block)))

;; Initialization of the game
(define (setup)
  (initscr) ; Initialize the screen
  (start_color) ; Start the terminal in color mode
  (noecho)  ; Turns off echoing of typed characters
  (cbreak)  ; Disable buffered of input
  (curs_set 0)  ; Hide the cursor
  (keypad (stdscr) #t)  ; Allow input on the screen
  (timeout TIME_OUT))   ; Set timeout

;; Restore the terminal values that have been overwritten
(define (destroy)
  (echo)
  (nocbreak)
  (curs_set 1)
  (endwin))

;; The entry into the game. Set up the terminal and out playing grid, and 
;; recurse until an exit status occurs.
(define (main)
  ;; Setup the terminal for the game
  (setup)

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
  (let ([workarea (make-array (shape 0 cols 0 lines))] ; Working grid
        [win (newwin 24 20 0 5)])               ; Display window
    (do ([i 0 (add1 i)])
      ((>= i cols))
      (do([j 0 (add1 j)])
        ((>= j lines))
        (array-set! workarea i j (cons 0 #f))))
        (tetra-display win workarea)

        ;;Start the main game loop
        (let loop ([continue #t] [newblock? #t] [block (L-block)])
          (unless (not continue)
            (if newblock?
              (begin 
                (set! block (move-block (new-tetra) 4 1))
                (set! newblock? #f))
              ;; Otherwise, remove the block from the workarea
              (update-state block #f workarea))
            
            ;; Update the block state based on input
            (let ([result (input block)])
              (if (boolean? result) 
                (set! continue result)  ; Exit out of our game loop   
                (set! block result)))   ; Continue with the loop
           
            (set! block (move-block block 0 1))
            ;; Update the state of the grid now
            (update-state block #t workarea)
            (tetra-display win workarea)
            (loop continue newblock? block))))    ; Keep looping
  ;; Restore the terminal values
  (destroy))
(main)
