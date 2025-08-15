#lang racket
(require sketching)

(provide draw-snowing)

(define snow-count 120)
(define snow (make-vector snow-count))

(define (reset-snow!)
  (for ([i (in-range snow-count)])
    (define x (random (width)))
    (define y (random (height)))
    (define r (+ 3 (random 5)))
    (define vy (+ 1.5 (random 1.5)))
    (vector-set! snow i (list x y r vy))))

(define (draw-snowing bg)
  (background bg)
  (when (or (not (= (vector-length snow) snow-count))
            (ormap (lambda (i) (not (list? (vector-ref snow i)))) (range snow-count)))
    (reset-snow!))
  (for ([i (in-range snow-count)])
    (define s (vector-ref snow i))
    (define x (list-ref s 0))
    (define y (list-ref s 1))
    (define r (list-ref s 2))
    (define vy (list-ref s 3))
    ;; Draw snowflake
    (no-stroke)
    (fill 255 140 0 180)
    (ellipse x y r r)
    ;; Update position
    (define ny (+ y vy))
    (define nx (+ x (random -1 2)))
    (if (> ny (height))
        (vector-set! snow i (list (random (width)) 0 r vy))
        (vector-set! snow i (list nx ny r vy)))))
