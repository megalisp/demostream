#lang racket
(require sketching)
(provide draw-boids)

(define boids-count 40)
(define boids-trail-length 30)
(define boids-margin 100)
(define boids-min-speed 2)
(define boids-size 16)
(define boids-separation-radius 32)
(define boids-alignment-radius 64)
(define boids-cohesion-radius 100)
(define boids-max-speed 3.5)
(define boids-max-force 0.08)

(define boids (make-vector boids-count))

(define (wrap v max)
  (let* ([m (abs max)]
         [r (- v (* m (floor (/ v m))))])
    (if (< r 0) (+ r m) r)))

(define (reset-boids!)
  (for ([i (in-range boids-count)])
    (define x (+ boids-margin (random (- (width) (* 2 boids-margin)))))
    (define y (+ boids-margin (random (- (height) (* 2 boids-margin)))))
    (define angle (* 2 pi (random)))
    (define speed (+ boids-min-speed (random (- boids-max-speed boids-min-speed))))
    (define vx (* speed (cos angle)))
    (define vy (* speed (sin angle)))
    (define trail (make-list boids-trail-length (list x y)))
    (vector-set! boids i (list x y vx vy trail))))


(define (boid-update i)
  (define my (vector-ref boids i))
  (define x (list-ref my 0))
  (define y (list-ref my 1))
  (define vx (list-ref my 2))
  (define vy (list-ref my 3))
  (define trail (list-ref my 4))
  ;; Flocking
  (define sep-x 0) (define sep-y 0) (define sep-count 0)
  (define ali-x 0) (define ali-y 0) (define ali-count 0)
  (define coh-x 0) (define coh-y 0) (define coh-count 0)
  (for ([j (in-range boids-count)] #:when (not (= i j)))
    (define other (vector-ref boids j))
    (define ox (list-ref other 0))
    (define oy (list-ref other 1))
    (define ovx (list-ref other 2))
    (define ovy (list-ref other 3))
    (define d (sqrt (+ (sqr (- x ox)) (sqr (- y oy)))))
    (when (< d boids-separation-radius)
      (set! sep-x (+ sep-x (/ (- x ox) (max d 0.01))))
      (set! sep-y (+ sep-y (/ (- y oy) (max d 0.01))))
      (set! sep-count (+ sep-count 1)))
    (when (< d boids-alignment-radius)
      (set! ali-x (+ ali-x ovx))
      (set! ali-y (+ ali-y ovy))
      (set! ali-count (+ ali-count 1)))
    (when (< d boids-cohesion-radius)
      (set! coh-x (+ coh-x ox))
      (set! coh-y (+ coh-y oy))
      (set! coh-count (+ coh-count 1))))
  ;; Calculate steering
  (define fx 0)
  (define fy 0)
  (when (> sep-count 0)
    (set! fx (+ fx (* 1.5 (let ([v (/ sep-x sep-count)]) (if (> (abs v) boids-max-force) (* (sgn v) boids-max-force) v)))))
    (set! fy (+ fy (* 1.5 (let ([v (/ sep-y sep-count)]) (if (> (abs v) boids-max-force) (* (sgn v) boids-max-force) v))))))
  (when (> ali-count 0)
    (set! fx (+ fx (* 1.0 (let ([v (/ ali-x ali-count)]) (if (> (abs v) boids-max-force) (* (sgn v) boids-max-force) v)))))
    (set! fy (+ fy (* 1.0 (let ([v (/ ali-y ali-count)]) (if (> (abs v) boids-max-force) (* (sgn v) boids-max-force) v))))))
  (when (> coh-count 0)
    (set! fx (+ fx (* 0.8 (let ([v (/ (- (/ coh-x coh-count) x))]) (if (> (abs v) boids-max-force) (* (sgn v) boids-max-force) v)))))
    (set! fy (+ fy (* 0.8 (let ([v (/ (- (/ coh-y coh-count) y))]) (if (> (abs v) boids-max-force) (* (sgn v) boids-max-force) v))))))
  ;; Update velocity
  (define nvx (let ([v (+ vx fx)]) (if (> (abs v) boids-max-speed) (* (sgn v) boids-max-speed) v)))
  (define nvy (let ([v (+ vy fy)]) (if (> (abs v) boids-max-speed) (* (sgn v) boids-max-speed) v)))
  ;; Update position
  (define nx (wrap (+ x nvx) (width)))
  (define ny (wrap (+ y nvy) (height)))
  ;; Update trail
  (define new-trail (cons (list nx ny) (take trail (- boids-trail-length 1))))
  (vector-set! boids i (list nx ny nvx nvy new-trail)))


;; Helper to check if a boid is valid (list of length 5)
(define (valid-boid? b)
  (and (list? b) (= (length b) 5)))


(define (draw-boids bg)
  (background bg)
  (when (or (not (= (vector-length boids) boids-count))
            (ormap (lambda (i) (not (valid-boid? (vector-ref boids i)))) (range boids-count)))
    (reset-boids!))
  (for ([i (in-range boids-count)])
    (boid-update i))
  (for ([i (in-range boids-count)])
    (define boid (vector-ref boids i))
    (when (valid-boid? boid)
      (define trail (list-ref boid 4))
      (define x (list-ref boid 0))
      (define y (list-ref boid 1))
      (define vx (list-ref boid 2))
      (define vy (list-ref boid 3))
      ;; Draw trail
      (stroke 255 140 0 80)
      (stroke-weight 4)
      (for ([j (in-range (- (length trail) 1))])
        (define p1 (list-ref trail j))
        (define p2 (list-ref trail (+ j 1)))
        (define dx (abs (- (car p1) (car p2))))
        (define dy (abs (- (cadr p1) (cadr p2))))
        (when (and (< dx (/ (width) 2)) (< dy (/ (height) 2)))
          (line (car p1) (cadr p1) (car p2) (cadr p2))))
      ;; Draw boid as triangle
  (let* ([angle (atan vy vx)]
     [size boids-size]
     [x1 (+ x (* size (cos angle)))]
     [y1 (+ y (* size (sin angle)))]
     [x2 (+ x (* size (cos (+ angle 2.5))))]
     [y2 (+ y (* size (sin (+ angle 2.5))))]
     [x3 (+ x (* size (cos (- angle 2.5))))]
     [y3 (+ y (* size (sin (- angle 2.5))))])
    (fill 255 140 0)
    (stroke 0)
    (triangle x1 y1 x2 y2 x3 y3)))))
