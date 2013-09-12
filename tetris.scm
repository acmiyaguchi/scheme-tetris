(declare (block) (fixnum) (usual-integrations))

(use ncurses srfi-25)
(load "tetromino.scm")

;; Tetris is scheme

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
(define display?-set! set-cdr!)

(define (tetra-display win area startx starty endx endy)
  (wclear win)
  (do ([i startx (add1 i)])
    ((>= i endx))
    (do ([j starty (add1 j)])
      ((>= j endy))
      (unless (not (display? (array-ref area i j)))
        (mvwaddch win j i CELL_CHAR))))
  (wrefresh win))

(define (update-state block disp area startx starty endx endy)
  (map (lambda (coord)
         ((unless (or (>= (car coord) endx)
                       (>= (cdr coord) endy)
                       (< (car coord) startx)
                       (< (cdr coord) starty))
           (let ([cell (array-ref area (car coord) (cdr coord))])
             (display?-set! (display? cell) disp)
             ;;TODO add in color information
            ))))
       block)
  
(define (main)
  ;;Settings for the terminal session
  (initscr)
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
  (let ([workarea (make-array (shape 0 cols 0 lines))])
    (do ([i 0 (add1 i)])
      ((>= i cols))
      (do([j 0 (add1 j)])
        ((>= j lines))
        (array-set! workarea i j (cons 0 #f))))
        (tetra-display (stdscr) workarea STARTX STARTY ENDX ENDY)
        (let loop ((continue #t)) ;;Start the main game loop
          (unless (not continue)
            (let blockloop ([newpos (L-block)])
              ;;(update-state newpos #t workarea STARTX STARTY ENDX ENDY)
              (case (getch)
                ((#\q) (set! continue #f))
                  )
                ;;(tetra-display (stdscr) workarea STARTX STARTY ENDX ENDY)
                (loop continue)))))
  (echo)
  (nocbreak)
  (curs_set 1)
  (endwin))

(main)
