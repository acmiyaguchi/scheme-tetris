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
(define (tetra-display win area startx starty endx endy)
  (wclear win)
  (do ([i startx (add1 i)])
    ((>= i endx))
    (do ([j starty (add1 j)])
      ((>= j endy))
      (let ([cell (array-ref area i j)])
        (unless (not (display? cell))
          (wattron win (COLOR_PAIR (color cell)))
          (mvwaddch win j i CELL_CHAR)
          (wattroff win (COLOR_PAIR (color cell)))))))
  (wrefresh win))

;; If disp is set to true, this function sets the array where the block
;; would be to be displayable. If false, erase the last position of the block
;; from the grid until we get a change in state
(define (update-state tetra disp area startx starty endx endy)
  (map (lambda (coord)
         (unless (or (>= (car coord) endx)
                       (>= (cdr coord) endy)
                       (< (car coord) startx)
                       (< (cdr coord) starty))
           (let ([cell (array-ref area (car coord) (cdr coord))])
             (display-set! cell disp)
             (color-set! cell (caar tetra)))))
       (calc-offset tetra)))

;; The entry into the game. Set up the terminal and out playing grid, and 
;; recurse until an exit status occurs.
(define (main)
  ;;Settings for the terminal session
  (initscr)     ;; Initialize the screen
  (start_color) ;; Start the terminal in color mode
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
    (unless (eqv? color COLOR_WHITE)
      (init_pair color color COLOR_BLACK)
      (set_colors (add1 color))))

  ;;Start setting up the work area
  (let ([workarea (make-array (shape 0 cols 0 lines))])
    (do ([i 0 (add1 i)])
      ((>= i cols))
      (do([j 0 (add1 j)])
        ((>= j lines))
        (array-set! workarea i j (cons 0 #f))))
        (tetra-display (stdscr) workarea STARTX STARTY ENDX ENDY)

        ;;Start the main game loop
        (let loop ([continue #t] [block (L-block)])
          (unless (not continue)
              (update-state block #f workarea STARTX STARTY ENDX ENDY)
              (case (getch)
                ((#\q) (set! continue #f))
                ((#\w) (set! block (move-block block 0 -1)))
                ((#\s) (set! block (move-block block 0 1)))
                ((#\a) (set! block (move-block block -1 0)))
                ((#\d) (set! block (move-block block 1 0)))
                ((#\space) (set! block (rot-cw block))))
              (update-state block #t workarea STARTX STARTY ENDX ENDY)
              (tetra-display (stdscr) workarea STARTX STARTY ENDX ENDY)
              (loop continue block))))
  (echo)
  (nocbreak)
  (curs_set 1)
  (endwin))

(main)
