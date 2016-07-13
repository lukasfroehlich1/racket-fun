#lang racket
(require rackunit)
(require racket/gui)
(require 2htdp/universe)
(require racket/draw)

(struct location (mis can) #:transparent)
; boat left is 0 boat right is 1
(struct state (left boat right hist) #:transparent)
(struct step (left boat right) #:transparent)


(define state0 (state (location 3 3) 0 (location 0 0) '()))

; final? determines if the final state has been reached
(define (final? val)
  (match val
    [(state left boat right hist)
     (and (and (equal? left (location 0 0))
          (equal? boat 1))
          (equal? right (location 3 3)))]))


(check-equal? (final? (state (location 0 0)
                             1
                             (location 3 3)
                             '())) #true)
              
(check-equal? (final? (state (location 1 0)
                             1
                             (location 2 3)
                             '())) #false)

(define (draw-people dc x y loc)
  (begin 
    (for ([mis (in-range (location-mis loc))])
      (send dc set-brush "white" 'solid)
      (send dc draw-ellipse (+ 3 x) (+ y (* 12 mis) 5) 10 10))
    (for ([mis (in-range (location-can loc))])
      (send dc set-brush "black" 'solid)
      (send dc draw-ellipse (+ 16 x) (+ y (* 12 mis) 5) 10 10))))

(define (draw-boat dc side)
  (cond
    [(= side 0)  (send dc draw-rectangle
                       37 20   ; Top-left at (0, 10), 10 pixels down from top-left
                       5 8)] ; 30 pixels wide and 10 pixels high)
    [else (send dc draw-rectangle
                       73 20   ; Top-left at (0, 10), 10 pixels down from top-left
                       5 8)])) ; 30 pixels wide and 10 pixels high)
    
(define (render-mc step)
  (begin 
    (define target (make-bitmap 200 100)) ; A 30x30 bitmap
    (define dc (new bitmap-dc% [bitmap target]))
    (send dc draw-rectangle
          5 5   ; Top-left at (0, 10), 10 pixels down from top-left
          30 90) ; 30 pixels wide and 10 pixels high
    (send dc draw-rectangle
          80 5   ; Top-left at (0, 10), 10 pixels down from top-left
          30 90) ; 30 pixels wide and 10 pixels high
    (draw-people dc 5 5 (step-left step))
    (draw-people dc 80 5 (step-right step))
    (draw-boat dc (step-boat step))
    target))


#;(render-mc (state (location 1 0)
                  0
                  (location 2 3)
                  '()))
#;(render-mc (state (location 3 3)
                  0
                  (location 0 0)
                  '()))

; valid-state checks to see if a state is valid
(define (valid-state game-state)
  (match game-state
    [(state left boat right hist)
     (and (not (ormap (λ (x) (equal? (state->step game-state) x)) hist))
          (or (= (location-mis left) 0)
              (>= (location-mis left) (location-can left)))
          (or (= (location-mis right) 0)
              (>= (location-mis right) (location-can right))))]))


; state->step converts a state into a step to be stored in a hist
(define (state->step game-state)
  (match game-state
    [(state left boat right hist) (step left boat right)]))

(check-equal? (valid-state (state
              (location 2 2)
              1
              (location 1 1)
              '())) #true)

(check-equal? (valid-state (state
              (location 3 2)
              1
              (location 0 1)
              '())) #true)

(check-equal? (valid-state (state
              (location 2 3)
              1
              (location 1 0)
              '())) #false)

(check-equal? (valid-state (state
              (location 2 2)
              1
              (location 1 1)
              (list (step (location 3 3) 0 (location 0 0))
                    (step (location 2 2) 0 (location 1 1))))) #true)

(check-equal? (valid-state (state
              (location 2 2)
              1
              (location 1 1)
              (list (step (location 3 3) 0 (location 0 0))
                    (step (location 2 2) 1 (location 1 1))
                    (step (location 2 2) 0 (location 1 1))))) #false)


; repeat-str consumes a string and len and creates a list of the given
; string repeated len times
(define (repeat-str str len)
  (cond
    [(= len 0) '()] 
    [else (cons str (repeat-str str (- len 1)))]))


; side->string converts a side to a string representation
(define (side->string side size)
  (append (repeat-str 'm
                      (if (> (location-mis side) size)
                          size 
                          (location-mis side)))
          (repeat-str 'c
                      (if (> (location-can side) size)
                          size 
                          (location-can side)))))

(check-equal? (side->string (location 3 2) 2)
              '(m m c c))


; count-char counts the number of instances of a char
(define (count-char side char)
  (match side
    ['() 0]
    [other (+ (count-char (rest side) char) (if (eq? char (first side)) 1 0))]))

; process-boat consumes a boat string and returns a boat state
(define (process-boat side)
  (location (count-char side 'm) (count-char side 'c)))


; apply-boat consumes a boat state, game state and returns a state that reflects
; the boat trip
(define (apply-boat game-state boat-state)
  (match game-state
    [(state left boat right hist)
     (cond
       [(= boat 0) (state
                    (location (- (location-mis left) (location-mis boat-state))
                              (- (location-can left) (location-can boat-state)))
                    1
                    (location (+ (location-mis right) (location-mis boat-state))
                              (+ (location-can right) (location-can boat-state)))
                    (cons (state->step game-state) hist))]
       [else (state
              (location (+ (location-mis left) (location-mis boat-state))
                        (+ (location-can left) (location-can boat-state)))
              0
              (location (- (location-mis right) (location-mis boat-state))
                        (- (location-can right) (location-can boat-state)))
              (cons (state->step game-state) hist))])]))

(check-equal? (apply-boat (state (location 1 2)
                                 0
                                 (location 2 1) '()) (location 1 1))
              (state (location 0 1)
                     1
                     (location 3 2) (list (step (location 1 2) 0 (location 2 1) ))))

(check-equal? (apply-boat (state (location 1 1)
                                 1
                                 (location 2 2) '()) (location 1 1))
              (state (location 2 2)
                     0
                     (location 1 1) (list (step (location 1 1) 1 (location 2 2)))))

; possible-boats-string consumes a side and returns a list of all possible boat-strings from that side
(define (possible-boats-string side size)
  (cond
    [(= size 0) '()]
    [else  (append (combinations (side->string side size) size) (possible-boats-string side (- size 1)))]))

; possible-boats consumes a list of boat strings and process' returning a list of boat-states
(define (possible-boats side size)
  (remove-duplicates (map process-boat (possible-boats-string side size))))

; create-next-states generates the next valid states from a set of states. considers
; states already reached in the history to avoid cycles
(define (create-next-states game-states boat-size)
  (remove-duplicates (append-map
                      (λ (game-state) 
                        (match game-state
                          [(state left boat right hist)
                           (cond
                             [(= boat 0) (filter valid-state (map (λ (x) (apply-boat game-state x)) (possible-boats left boat-size)))]
                             [else (filter valid-state (map (λ (x) (apply-boat game-state x)) (possible-boats right boat-size)))])]))
                      game-states)))

(define (solve state0 final? boat-size)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generate the successor states for all intermediate ones
          (define (solve* los)
            (cond
              [(ormap final? los) (first (filter final? los))]
              [(eq? (length los) 0) (error "no valid solution found")]
              [else (solve* (create-next-states los boat-size))])))
    (solve* (list state0))))


(define (final-new? val)
  (match val
    [(state left boat right hist)
     (and (and (equal? left (location 0 0))
          (equal? boat 1))
          (equal? right (location 5 4)))]))

(define state1 (state (location 5 4) 0 (location 0 0) '()))
(define res (solve state1 final-new? 2))
(run-movie 0.5 (map render-mc (reverse (cons (state->step res) (state-hist res)))))



