#lang racket
(require rackunit)
(require racket/gui)
(require 2htdp/universe)
(require racket/draw)

; Currently incomplete. need to implement minimax 
 

(struct game-state (board turn hist) #:transparent)
(struct step (board turn) #:transparent)

(define board_test '((x o x) (o x -) (x - -)))
(define board0 '((- - -) (- - -) (- - -)))
(define game-state0 (game-state board0 'x '()))


; state->step consumes a state and converts into a step
(define (state->step state)
  (match state
    [(game-state board turn hist) (step board turn)]))

(check-equal? (state->step game-state0) (step '((- - -) (- - -) (- - -)) 'x))


; get-diagonal consumes a board and returns a list of each diagonal
(define (get-diagonal board length)
  (cond
    [(< length 0) '()]
    [else (cons (list-ref (list-ref board length) length)
                (get-diagonal board (- length 1)))]))

; get-left-diagonal consumes a board and returns a list of each diagonal
(define (get-left-diagonal board len)
  (cond
    [(< len 0) '()]
    [else (cons (list-ref (list-ref board len) (- (- (length board) 1) len))
                (get-left-diagonal board (- len 1)))]))


; check-slice consumes a list of locations and check if all equal a given piece
(define (check-slice slice piece)
  (andmap (λ (x) (eq? x piece)) slice))


; check-diagonal consumes a board and a piece and checks to see if the piece has won on the diagonal
(define (check-diagonal board piece)  
  (or (check-slice (get-diagonal board (- (length board) 1)) piece)
      (check-slice (get-left-diagonal board (- (length board) 1)) piece)))

(check-equal? (check-diagonal board_test 'x) #true)
(check-equal? (check-diagonal board_test 'o) #false)


; check-rows consumes a board and a piece and checks to see if the piece has won in a row
(define (check-rows board piece)
  (ormap (λ (x) (check-slice x piece)) board))

(check-equal? (check-rows board_test 'x) #false)
(check-equal? (check-rows '((x x x) (x o o) (o - -)) 'x) #true)
(check-equal? (check-rows '((x o x) (x o o) (x - -)) 'o) #false)


; check-cols consumes a board and a piece and checks to see if the piece has won in a col
(define (check-cols board piece)
  (for/or ([fst (first board)]
           [snd (second board)]
           [thd (third board)])
    (check-slice (list fst snd thd) piece)))

(check-equal? (check-cols '((x o x) (x o o) (x - -)) 'x) #true)
(check-equal? (check-cols board_test 'x) #false)


; final? consumes a board state and checks to see if a piece has won
(define (final? state)
  (match state
    [(game-state board turn hist)
     (begin
       (define piece (if (eq? turn 'x) 'o 'x))
       (or (check-diagonal board piece)
           (check-rows board piece)
           (check-cols board piece)))]))

(check-equal? (final? (game-state board_test 'o '())) #true)
(check-equal? (final? game-state0) #false)
(check-equal? (final? (game-state '((x - -) (- - -) (- - -)) 'o '())) #false)
(check-equal? (final? (game-state '((x - -) (- - -) (- - -)) 'x '())) #false)

; board->string consumes a board and flattens it into a string
(define (board->string board)
  (match board
    ['() '()]
    [(cons fst rst) (append fst (board->string rst))]))

(check-equal? (board->string board_test) '(x o x o x - x - -))
  

; string->board consumes a string representing a board and constructs it into a board
(define (string->board str)
  (cond
    [(= (length str) 0) '()]
    [else (append (list (take str 3)) (string->board (drop str 3)))]))

(check-equal? (string->board '(x o x o x - x - -)) board_test)


; find-empty consumes a board-string and returns a list of the locations where the board is empty
(define (find-empty str pos)
  (match str 
    ['() '()]
    [(cons fst rst) (if (eq? fst '-)
                        (cons pos (find-empty rst (+ pos 1)))
                        (find-empty rst (+ pos 1)))]))

(check-equal? (find-empty '(x o - x -) 0) (list 2 4))
(check-equal? (find-empty '(x o x x o) 0) '())


; replace-pos consumes a list, position, value to swap into the list at the given position
(define (replace-pos list pos val)
  (append (take list pos) (cons val (drop list (+ pos 1)))))

(check-equal? (replace-pos '(x - o x x) 1 'k) '(x k o x x))
(check-equal? (replace-pos '(x - o x x) 0 'k) '(k - o x x))


; generate-board consumes a board, piece and generates all possible boards after the piece has played
(define (generate-board board piece)
  (for/list ([empty-spot (find-empty (board->string board) 0)])
    (string->board (replace-pos (board->string board) empty-spot piece))))

(check-equal? (generate-board board_test 'x) (list '((x o x) (o x x) (x - -))
                                                   '((x o x) (o x -) (x x -))
                                                   '((x o x) (o x -) (x - x))))


; interpret-boards consumes the previous state, a list of the new boards and returns a list of valid next states
(define (interpret-boards prev-state boards)
  (map (λ (board) (game-state board 
                              (if (eq? (game-state-turn prev-state) 'x)
                                  'o
                                  'x)
                              (cons (state->step prev-state) (game-state-hist prev-state))))
       boards))
       

; create-next-states consumes a list of current states and produces a list of valid next states
(define (create-next-states states)
  (append-map (λ (state)
                (interpret-boards state (generate-board (game-state-board state) (game-state-turn state))))
              states))



;solve cosumes a current state and 
(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generate the successor states for all intermediate ones
          (define (solve* los)
            (cond
              [(ormap final? los) (first (filter final? los))]
              [(eq? (length los) 0) (error "no valid solution found")]
              [else (solve* (create-next-states los))])))
    (solve* (list state0))))


;(solve game-state0)

(define board1 '((- - -) (- x -) (- - -)))
(define game-state1 (game-state board1 'o '()))

(solve (game-state '((o - -) (- x -) (- x -)) 'o '()))
                                






