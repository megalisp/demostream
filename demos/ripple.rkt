#lang racket
(require sketching)
(provide draw-ripple)

(define ripple-max-total 5)
(define ripple-min-max-r 300)
(define ripple-max-max-r 500)
(define ripple-min-alpha 255)(provide draw-ripple)

(define ripple-weight 5)
(define ripple-ring-step 40)
(define ripple-ring-count 6)
(define ripples (box '()))


(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))



;; Spawn at random locations

(define (add-ripple)
  (when (< (length (unbox ripples)) ripple-max-total)
    (let try-place ()
      (let* ([x (random (width))]
             [y (random (height))]
             [r 0]
             [max-r (+ ripple-min-max-r (random (- ripple-max-max-r ripple-min-max-r)))]
             [alpha ripple-min-alpha]
             [ok? (for/and ([v (in-list (unbox ripples))])
                     (> (distance x y (vector-ref v 0) (vector-ref v 1))
                        (+ max-r (vector-ref v 3))))])
        (if ok?
            (set-box! ripples (cons (vector x y r max-r alpha) (unbox ripples)))
            (try-place))))))


;; Remove any ripples that overlap (touch) after update
(define (update-ripples)
  (let* ([updated
           (map (lambda (v)
                  (let* ([x (vector-ref v 0)]
                         [y (vector-ref v 1)]
                         [r (add1 (vector-ref v 2))]
                         [max-r (vector-ref v 3)]
                         [alpha (max 0 (- (vector-ref v 4) 4))])
                    (vector x y r max-r alpha)))
                (unbox ripples))]
         [filtered (filter (lambda (v) (< (vector-ref v 2) (vector-ref v 3))) updated)]
         [to-remove (let ([n (length filtered)])
                      (for*/set ([i (in-range n)] [j (in-range (+ i 1) n)])
                        (let* ([v1 (list-ref filtered i)]
                               [v2 (list-ref filtered j)]
                               [d (distance (vector-ref v1 0) (vector-ref v1 1)
                                            (vector-ref v2 0) (vector-ref v2 1))]
                               [r1 (+ (vector-ref v1 2) (vector-ref v1 3))]
                               [r2 (+ (vector-ref v2 2) (vector-ref v2 3))])
                          (when (<= d (+ (vector-ref v1 2) (vector-ref v2 2)))
                            (values i j)))))]
         [indices-to-remove (flatten (set->list to-remove))])
    (set-box! ripples
      (for/list ([v filtered] [idx (in-naturals)] #:unless (member idx indices-to-remove)) v))))

;; Accepts a background color argument
(define (draw-ripples bg)
  (background bg)
  (for ([v (in-list (unbox ripples))])
    (let* ([x (vector-ref v 0)]
           [y (vector-ref v 1)]
           [r (vector-ref v 2)]
           [max-r (vector-ref v 3)]
           [alpha (vector-ref v 4)])
      (no-fill)
      (stroke (color 255 140 0)) ; solid orange, no alpha gradient
  (stroke-weight ripple-weight)
      (for ([i (in-range 0 ripple-ring-count)]) ; more rings
        (ellipse x y (* 2 (+ r (* i ripple-ring-step))) (* 2 (+ r (* i ripple-ring-step))))))))

(define (draw-ripple bg)
  (when (< (random 1.0) 0.08) (add-ripple))
  (update-ripples)
  (draw-ripples bg))

