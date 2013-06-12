(define board_def '(
0 0 0 0 0 0 1
0 0 0 0 0 2 3
0 2 0 0 0 0 0 
0 0 0 4 5 0 0 
0 0 4 0 6 0 0 
0 0 0 0 3 6 0 
0 0 0 0 0 1 5
))
(define board_def2 '( 
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

(define board_size (length board_def))
(define board_occ (make-list board_size 0))
(define num_colors (apply max board_def))
(define board_width (inexact->exact (sqrt board_size)))

(define (findPipeStart color )
    (list-index board_def color))

(define (isSolved board_occupied)
    (not (list-index board_occupied 0)))

(define (colorAt index)
    (list-ref board_def index))

(define (isOccupied pos board_occupied) 
    (and pos (not (= 0 (list-ref board_occupied pos)))))

(define (printBoard board)
    (define board_width (inexact->exact (sqrt (length board))))
    (let loop_x ((i 0))
        ( if ( = i board_width) #f (begin 
            (let loop_y ((j 0))
                (if ( = j board_width) #f (begin
                    (display (list-ref board (+ j (* board_width i) )))
                    (loop_y (+ 1 j)))))
            (display "\n")
            (loop_x (+ i 1)))))
    (display "\n"))

(define (isFinishedPipe color board_occupied)
    (define first (findPipeStart color))
	(if (not (isOccupied first board_occupied))
		#f (let loop ((i (+ 1 first)))
			(if (>= i board_size)
				#f (if (and (isOccupied i board_occupied) (= color (colorAt i)))
					#t (loop (+ i 1)))))))

(define (markBoard pos color board_occupied )
    (let ((board (list-copy board_occupied)))
        (list-set! board pos color)
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

(define (explore pos color board_occupied)
    (let ((board_copy (list-copy board_occupied)))
        (cond
            ((and (canGoNorth pos color board_copy) (solve (- pos board_width) color board_copy)) board_copy)
            ((and (canGoSouth pos color board_copy) (solve (+ pos board_width) color board_copy)) board_copy)
            ((and (canGoEast  pos color board_copy) (solve (+ pos 1          ) color board_copy)) board_copy)
            ((and (canGoWest  pos color board_copy) (solve (- pos 1          ) color board_copy)) board_copy)
            (#t #f)))) ;; default case, none led to a solution so return false

(define (explore pos color board_occupied)
    (define board_copy (list-copy board_occupied))
	(cond
		((and (canGoNorth pos color board_copy) (solve (- pos board_width) color board_copy)) board_copy)
		((and (canGoSouth pos color board_copy) (solve (+ pos board_width) color board_copy)) board_copy)
		((and (canGoEast  pos color board_copy) (solve (+ pos 1          ) color board_copy)) board_copy)
		((and (canGoWest  pos color board_copy) (solve (- pos 1          ) color board_copy)) board_copy)
		(#t #f))) ;; default case, none led to a solution so return false			
			
;; Main solver, recursive function.
(define (solve pos color board_occupied)
    (define new_board (markBoard pos color board_occupied))
    (if (isSolved new_board)
        (begin 
			(printBoard new_board)
			(display "solved!")
            new_board );; it's solved, so return board as is.
		(if (isFinishedPipe color new_board) ;;
			(if (>= color num_colors) 
				#f                           ;; pipe is finished, but board is not solved and no more colors.
				(let* ((new_color (+ 1 color)) (new_pos (findPipeStart new_color))) ;; pipe is finished and there are more colors
					(solve new_pos new_color new_board ))) ;; continue to solve from start of next pipe
			(explore pos color new_board))))

(define (flow_solve)
    (define start-time (tms:clock (times )))
    (solve (findPipeStart 1) 1 board_occ)
    (let ((total-time (exact->inexact (/ (- (tms:clock (times )) start-time)  internal-time-units-per-second ))))
        (display (string-join `("\n" ,(number->string total-time) "\n" )))
    )
)
    

;;(flow_solve) ;; run the solver
    
    
(define (aloop)
    (let loop_x ((i 0))
        ( if ( = i 3) #f (begin 
            (let loop_y ((j 0))
                (if ( = j 3) #f (begin
                    (display j)
                    (loop_y (+ 1 j)))))
            (display "\n")
            (loop_x (+ i 1))))))

