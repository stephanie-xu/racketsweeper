;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname minesweeper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct point [row col])
(define PT-1 (make-point 1 1))
(define PT-2 (make-point 2 2))
(define PT-3 (make-point 1 2))
(define PT-4 (make-point 2 3))

(define-struct bomb [pt])
(define BOMB-1 (make-bomb PT-1))
(define BOMB-2 (make-bomb PT-2))

(define-struct land [pt clicked])
(define LAND-1 (make-land PT-3 #f))
(define LAND-2 (make-land PT-4 #t))

(define (generate-random row col)
  (local [(define r (random 5))]
    (if (or (= 0 r) (= 1 r)) (make-bomb (make-point row col)) (make-land (make-point row col) #f))))

(define (generate-row row col bomb-limit)
  (local [(define new-tile (if (= bomb-limit 0) (make-land (make-point row col) #f) (generate-random row col)))
          (define new-limit (if (or (= bomb-limit 0) (land? new-tile)) bomb-limit (- bomb-limit 1)))]
    (cond
      [(= col 0) '()]
      [(> col 0) (cons new-tile (generate-row row (- col 1) new-limit))])))

(define (count-bombs row)
  (foldr (lambda (tile acc) (if (bomb? tile) (+ 1 acc) acc)) 0 row))

(check-expect (count-bombs (list BOMB-1 BOMB-2)) 2)
(check-expect (count-bombs '()) 0)
(check-expect (count-bombs (list LAND-2 BOMB-1 LAND-1)) 1)

(define (generate-board r c bomb-limit)
  (local [(define new-row (generate-row r c bomb-limit))
          (define new-limit (if (= bomb-limit 0) bomb-limit (- bomb-limit (count-bombs new-row))))]
    (cond
      [(= r 0) '()]
      [(> r 0) (cons new-row (generate-board (- r 1) c new-limit))])))

(define (get-possible-neighbors row col)
  (cons (make-point (- row 1) (- col 1))
        (make-point (- row 1) col)
        (make-point (- row 1) (+ col 1))
        (make-point row (- col 1))
        (make-point row (+ col 1))
        (make-point (+ row 1) (- col 1))
        (make-point (+ row 1) col)
        (make-point (+ row 1) (+ col 1))
        '()))

(define (same-point tile row col)
  (local [(define selector (if (bomb? tile) bomb-pt land-pt))]
  (and (= row (point-row (selector tile))) (= col (point-col (selector tile))))))
(check-expect (same-point BOMB-1 1 2) #f)
(check-expect (same-point LAND-1 1 2) #t)

(define (same-row tile row)
  (cond
    [(bomb? tile) (= row (point-row (bomb-pt tile)))]
    [(land? tile) (= row (point-row (land-pt tile)))]))
(check-expect (same-row BOMB-1 2) #f)
(check-expect (same-row LAND-1 1) #t)

(define (is-bomb row col grid)
  (local [(define (bomb-row row col row-lst)
            (ormap (lambda (tile) (and (same-point tile row col) (bomb? tile))) row-lst))]
  (cond
    [(empty? grid) #f]
    [(cons? grid) (if (same-row (first (first grid)) row)
                      (bomb-row row col (first grid))
                      (is-bomb row col (rest grid)))])))

#|(define (num-bombs row col)
  (local [(define neighbors (get-possible-neighbors row col))]
    (foldr 
|#