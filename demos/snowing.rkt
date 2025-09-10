#lang racket
(require sketching)
(provide draw-snowing)

(define snowing-count 120)
(define snowing-radius-min 3)
(define snowing-radius-max 8)
(define snowing-vy-min 1.5)
(define snowing-vy-max 3.0)
(define snowing-snow (make-vector snowing-count))


(define (reset-snow!)
  (for ([i (in-range snowing-count)])
    (define x (random (width)))
    (define y (random (height)))
    (define r (+ snowing-radius-min (random (- snowing-radius-max snowing-radius-min))))
    (define vy (+ snowing-vy-min (random (- snowing-vy-max snowing-vy-min))))
    (vector-set! snowing-snow i (list x y r vy))))


(define (draw-snowing bg)
  (background bg)
  (when (or (not (= (vector-length snowing-snow) snowing-count))
            (ormap (lambda (i) (not (list? (vector-ref snowing-snow i)))) (range snowing-count)))
    (reset-snow!))
  (for ([i (in-range snowing-count)])
    (define s (vector-ref snowing-snow i))
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
        (vector-set! snowing-snow i (list (random (width)) 0 r vy))
        (vector-set! snowing-snow i (list nx ny r vy)))))
