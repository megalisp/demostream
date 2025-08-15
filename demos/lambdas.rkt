#lang racket
(require sketching)
(provide draw-lambdas)


(define lambda-count 40)

;; Each lambda: (list x y size alpha fade-in fade-out state)
;; state: 'in or 'out
(define lambdas (make-vector lambda-count))



(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

(define (non-overlapping-position size existing)
  (let loop ()
    (define x (random (max 1 (- (width) size))))
    (define y (random (max 1 (- (height) size))))
    (if (for/and ([e existing])
          (> (distance x y (list-ref e 0) (list-ref e 1))
             (* 0.7 (+ size (list-ref e 2)))))
        (list x y)
        (loop))))

(define (reset-lambdas!)
  (let ([w (max 1 (width))]
        [h (max 1 (height))])
    (define existing '())
    (for ([i (in-range lambda-count)])
      (define size (+ 32 (random 120)))
      (define pos (non-overlapping-position size existing))
      (define fade-in (+ 2 (random 8)))
      (define fade-out (+ 2 (random 8)))
  (define l (list (car pos) (cadr pos) size 0 fade-in fade-out 'in))
      (vector-set! lambdas i l)
      (set! existing (cons l existing)))))


(define (setup)
  (size 800 600)
  (reset-lambdas!))


  
(define (draw-lambdas bg)
  (background bg)
  (when (or (not (= (vector-length lambdas) lambda-count))
            (not (list? (vector-ref lambdas 0))))
    (reset-lambdas!))
  (define all-done? #t)
  (for ([i (in-range lambda-count)])
    (define l (vector-ref lambdas i))
    (when (and (list? l) (= (length l) 7))
      (define x (list-ref l 0))
      (define y (list-ref l 1))
      (define s (list-ref l 2))
      (define a (list-ref l 3))
      (define fade-in (list-ref l 4))
      (define fade-out (list-ref l 5))
      (define state (list-ref l 6))
      (fill 255 140 0 a)
      (no-stroke)
      (text-size s)
      (text "λ" x y)
      (cond
        [(eq? state 'in)
         (define new-a (min 255 (+ a fade-in)))
         (vector-set! lambdas i (list x y s new-a fade-in fade-out (if (>= new-a 255) 'out 'in)))]
        [(eq? state 'out)
         (define new-a (max 0 (- a fade-out)))
         (vector-set! lambdas i (list x y s new-a fade-in fade-out (if (<= new-a 0) 'done 'out)))])
      (when (not (eq? (list-ref (vector-ref lambdas i) 6) 'done))
        (set! all-done? #f))))
  (when all-done?
    (reset-lambdas!)))

