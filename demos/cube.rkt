#lang racket
(require sketching)
(provide draw-cube)

(define cube-angle 0)
(define cube-size 200)
(define cube-cx (/ 1920 2))
(define cube-cy (/ 1080 2))
(define cube-weight 8)


(define cube-verts '(( 1  1  1) ( 1  1 -1) ( 1 -1  1) ( 1 -1 -1)
                     (-1  1  1) (-1  1 -1) (-1 -1  1) (-1 -1 -1)))
(define cube-edges '((0 1) (0 2) (0 4) (1 3) (1 5) (2 3) (2 6)
                     (3 7) (4 5) (4 6) (5 7) (6 7)))


(define (rotate-x v theta)
  (let* ([x (list-ref v 0)] [y (list-ref v 1)] [z (list-ref v 2)]
         [y2 (- (* y (cos theta)) (* z (sin theta)))]
         [z2 (+ (* y (sin theta)) (* z (cos theta)))])
    (list x y2 z2)))


(define (rotate-y v theta)
  (let* ([x (list-ref v 0)] [y (list-ref v 1)] [z (list-ref v 2)]
         [z2 (- (* z (cos theta)) (* x (sin theta)))]
         [x2 (+ (* z (sin theta)) (* x (cos theta)))])
    (list x2 y z2)))


(define (project v size cx cy)
  (let ([x (list-ref v 0)] [y (list-ref v 1)])
    (list (+ cx (* x size)) (+ cy (* y size)))))


(define (draw-cube bg)
  (background bg)
  (define cx cube-cx)
  (define cy cube-cy)
  (define sz cube-size)
  (define rotated (map (λ (v) (rotate-y (rotate-x v cube-angle) cube-angle)) cube-verts))
  (define projected (map (λ (v) (project v sz cx cy)) rotated))
  (stroke (color 255 140 0))
  (stroke-weight cube-weight)
  (for ([e cube-edges])
    (define va (list-ref projected (car e)))
    (define vb (list-ref projected (cadr e)))
    (apply line (append va vb)))
  (set! cube-angle (+ cube-angle 0.03)))
