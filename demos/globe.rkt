#lang racket
(require sketching)
(provide draw-globe)

(define globe-radius 350)
(define globe-angle 0)
(define globe-cx (/ 1920 2))
(define globe-cy (/ 1080 2))
(define globe-weight 8)


(define (draw-globe bg)
  (background bg)
  (define cx globe-cx)
  (define cy globe-cy)
  (stroke (color 255 140 0))
  (stroke-weight globe-weight)
  (for ([lat (in-range -80 81 10)])
    (define prev #f)
    (for ([lon (in-range -180 181 10)])
      (define pt (let* ([lat-rad (* lat (/ pi 180.0))]
                        [lon-rad (* lon (/ pi 180.0))]
                        [x (* globe-radius (cos lat-rad) (cos lon-rad))]
                        [y (* globe-radius (sin lat-rad))]
                        [z (* globe-radius (cos lat-rad) (sin lon-rad))]
                        [theta globe-angle]
                        [z2 (- (* z (cos theta)) (* x (sin theta)))]
                        [x2 (+ (* z (sin theta)) (* x (cos theta)))])
                   (list x2 y z2)))
      (define proj (let ([x (list-ref pt 0)] [y (list-ref pt 1)]) (list (+ cx x) (+ cy y))))
      (when prev (line (car prev) (cadr prev) (car proj) (cadr proj)))
      (set! prev proj)))
  (for ([lon (in-range -180 181 10)])
    (define prev #f)
    (for ([lat (in-range -90 91 10)])
      (define pt (let* ([lat-rad (* lat (/ pi 180.0))]
                        [lon-rad (* lon (/ pi 180.0))]
                        [x (* globe-radius (cos lat-rad) (cos lon-rad))]
                        [y (* globe-radius (sin lat-rad))]
                        [z (* globe-radius (cos lat-rad) (sin lon-rad))]
                        [theta globe-angle]
                        [z2 (- (* z (cos theta)) (* x (sin theta)))]
                        [x2 (+ (* z (sin theta)) (* x (cos theta)))])
                   (list x2 y z2)))
      (define proj (let ([x (list-ref pt 0)] [y (list-ref pt 1)]) (list (+ cx x) (+ cy y))))
      (when prev (line (car prev) (cadr prev) (car proj) (cadr proj)))
      (set! prev proj)))
  (set! globe-angle (+ globe-angle 0.02)))
