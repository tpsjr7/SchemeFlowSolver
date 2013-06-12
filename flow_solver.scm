(define board_def #(
1 2 0 
0 2 0 
0 0 1
))
(define board_def3 #(
0 0 0 0 0 0 1
0 0 0 0 0 2 3
0 2 0 0 0 0 0 
0 0 0 4 5 0 0 
0 0 4 0 6 0 0 
0 0 0 0 3 6 0 
0 0 0 0 0 1 5
))
(define board_def2 #( 
0 0 0 0 0 0 0 0 1
0 2 0 0 0 0 0 0 2
0 1 3 0 0 4 0 5 6
7 0 0 7 0 0 0 0 0
5 0 0 3 0 0 0 6 8
8 9 0 4 0 0 0 9 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
))

(define board_size (vector-length board_def))
(define board_occ (make-vector board_size 0))
(define num_colors (apply max (vector->list board_def)))
(define board_width (exact-integer-sqrt board_size))

;; http://www.gnu.org/software/guile/manual/html_node/while-do.html
(define pipePairs 
    (let ((pairs (make-vector (+ 1 num_colors) '())))
        (let loop ((i 0)) 
            (unless (= i board_size)
                (let ((color (vector-ref board_def i)))
                    (if (not (= 0 color))
                        (vector-set! pairs color (cons i (vector-ref pairs color)))))
                (loop (1+ i))))
        ;; convert list pairs into vector pairs
        (let loop ((i 0))
            (unless (> i num_colors)
                (vector-set! pairs i (list->vector (vector-ref pairs i)))
                (loop (1+ i))))
        pairs))

(define (findPipeStart color )
    (vector-ref (vector-ref pipePairs color) 0))

(define (isSolved count) ;;TODO fix caller
    (>= count board_size))

(define (colorAt index)
    (vector-ref board_def index))

(define (isOccupied pos board_occupied) 
    (and pos (not (= 0 (vector-ref board_occupied pos)))))

(define (printBoard board)
    (define board_width (inexact->exact (sqrt (vector-length board))))
    (let loop_x ((i 0))
        ( if ( = i board_width) #f (begin 
            (let loop_y ((j 0))
                (if ( = j board_width) #f (begin
                    (display (vector-ref board (+ j (* board_width i) )))
                    (loop_y (+ 1 j)))))
            (display "\n")
            (loop_x (+ i 1)))))
    (display "\n"))

(define (isFinishedPipe color board_occupied)
    (define apair (vector-ref pipePairs color))
    (and 
        (not(= 0 (vector-ref board_occupied (vector-ref apair 1)))) ;; check 1th first because 0th is most likely there.
        (not(= 0 (vector-ref board_occupied (vector-ref apair 0))))))
        

(define (markBoard pos color board_occupied )
    (let ((board (vector-copy board_occupied)))
        (vector-set! board pos color)
        board))

(define (isPosCompat color new_pos board_occupied) ;; is position compatible
    (let ((new_color (colorAt new_pos )))
        (and (or (= color new_color) (= 0 new_color)) (not (isOccupied new_pos board_occupied)))))
        
   
(define (canGoNorth pos color board_occupied)
    (and (>= pos board_width)                           (isPosCompat color (- pos board_width) board_occupied)))
       
(define (canGoSouth pos color board_occupied)
    (and (< pos (- board_size board_width))             (isPosCompat color (+ pos board_width) board_occupied)))

(define (canGoEast pos color board_occupied)
    (and (< (modulo pos board_width) (- board_width 1)) (isPosCompat color (+ pos 1)           board_occupied)))

(define (canGoWest pos color board_occupied)
    (and (> (modulo pos board_width) 0)                 (isPosCompat color (- pos 1)           board_occupied)))

(define (explore pos color board_occupied move_count )
    (define new_move_count (+ 1 move_count))
    ;(define y  (inexact->exact ( floor  ( / pos board_width))))
    ;(define x  (- pos         ( * y  board_width )))
    ;(define ty (inexact->exact ( floor  ( / target_pos board_width))))
    ;(define tx (- target_pos  ( * ty board_width )))
    (define (tryDirection directionFunc new_pos)
        (and (directionFunc pos color board_occupied) (solve new_pos color board_occupied new_move_count)))

    (define (GN) (tryDirection canGoNorth (- pos board_width)))
    (define (GS) (tryDirection canGoSouth (+ pos board_width)))    
    (define (GE) (tryDirection canGoEast  (+ pos 1          )))
    (define (GW) (tryDirection canGoWest  (- pos 1          )))

	(if (or (GN) (GS) (GE) (GW)) board_occupied  #f))
			
;; Main solver, recursive function.
(define (solve pos color board_occupied move_count)
    (define new_board (markBoard pos color board_occupied))
    ;;(printBoard new_board)
    (if (isSolved move_count)
        (begin 
            (printBoard new_board)
			(display "solved!")
            new_board );; it's solved, so return board as is.
		(if (isFinishedPipe color new_board) ;;
			(if (>= color num_colors) 
				#f                           ;; pipe is finished, but board is not solved and no more colors.
				(let* ((new_color (+ 1 color)) (new_pos (findPipeStart new_color))) ;; pipe is finished and there are more colors
					(solve new_pos new_color new_board (+ 1 move_count )))) ;; continue to solve from start of next pipe
			(explore pos color new_board move_count))))

(define (flow_solve)
    (define start-time (tms:clock (times )))
    (solve (findPipeStart 1) 1 board_occ 1)
    (let ((total-time (exact->inexact (/ (- (tms:clock (times )) start-time)  internal-time-units-per-second ))))
        (display (string-join `("\n" ,(number->string total-time) "\n" )))
    )
)
    

;;(flow_solve) ;; run the solver
    
    
(define (aloop)
    (let loop_x ((i 0)) ( if ( = i 3) #f (begin 
        (let loop_y ((j 0)) (if ( = j 3) #f (begin
            (display j)
            (loop_y (+ 1 j)))))
        (display "\n")
        (loop_x (+ i 1))))))

(let loop ((i 0)(j 5))
    (unless (= i 10)
        (format #t "i is ~a, j is ~a~%" i j)
        (loop (+ 1 i) 20 )))

(define (anotherloop)
    (do ((i 0 (+ i 1)))
        ((= i 10))
        (display i)))


