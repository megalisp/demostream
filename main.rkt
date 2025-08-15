#!/usr/bin/env racket
#lang sketching

(require "demos/demos.rkt")


(define WIDTH 1920)
(define HEIGHT 1080)
(define FPS 24)

(define ORANGE (color 255 140 0))
(define WHITE 255)
(define BLACK (color 0 0 0))
(define GREEN-SCREEN (color 0 255 0))
(define bg-color (box BLACK))


(define demos-list (list 'globe 'cube 'waves 'lissajous 'starfield 'spiral 'tunnel 'metaballs 'smiley 'lambdas 'pipes 'boids 'snowing 'dvd 'ripple))

(define (shuffle-list lst)
  (let* ([v (list->vector lst)]
         [n (vector-length v)])
    (for ([i (in-range (- n 1) 0 -1)])
      (define i-int (if (exact-integer? i) i (inexact->exact (floor i))))
      (define j (inexact->exact (floor (random (+ i-int 1)))))
      (when (or (not (exact-integer? i-int)) (not (exact-integer? j)))
        (displayln (format "DEBUG: i=~a j=~a" i-int j)))
      (let ([tmp (vector-ref v i-int)])
        (vector-set! v i-int (vector-ref v j))
        (vector-set! v j tmp)))
    (vector->list v)))

(define shuffled-demos (box (shuffle-list demos-list)))
(define demo-index (box 0))
(define last-demo (box #f))
(define last-switch (box (current-inexact-milliseconds)))
(define next-switch (box (+ (unbox last-switch) (+ 10000 (random 20000)))))



(define (draw)
  (define now (current-inexact-milliseconds))
  (when (> now (unbox next-switch))
    (set-box! demo-index (+ (unbox demo-index) 1))
    (when (>= (unbox demo-index) (length (unbox shuffled-demos)))
      (set-box! shuffled-demos (shuffle-list demos-list))
      (set-box! demo-index 0))
    (set-box! last-switch now)
    (set-box! next-switch (+ now (+ 10000 (random 20000)))) )
  (define current-demo (list-ref (unbox shuffled-demos) (unbox demo-index)))
  (when (not (equal? (unbox last-demo) current-demo))
    (displayln (string-append "Active simulation: " (symbol->string current-demo)))
    (set-box! last-demo current-demo))


  (case current-demo
    [(globe) (draw-globe (unbox bg-color))]
    [(cube) (draw-cube (unbox bg-color))]
    [(waves) (draw-waves (unbox bg-color))]
    [(lissajous) (draw-lissajous (unbox bg-color))]
    [(starfield) (draw-starfield (unbox bg-color))]
    [(spiral) (draw-spiral (unbox bg-color))]
    [(tunnel) (draw-tunnel (unbox bg-color))]
    [(metaballs) (draw-metaballs WIDTH HEIGHT (unbox bg-color))]
    [(smiley) (draw-smiley (unbox bg-color))]
    [(lambdas) (draw-lambdas (unbox bg-color))]
    [(pipes) (draw-pipes (unbox bg-color))]
    [(boids) (draw-boids (unbox bg-color))]
    [(snowing) (draw-snowing (unbox bg-color))]
    [(dvd) (draw-dvd)]
    [(ripple) (draw-ripple-demo)]))


(define (on-key-pressed)
  (when (char? key)
    (cond
      [(char=? key #\space)
       (set-box! demo-index (+ (unbox demo-index) 1))
       (when (>= (unbox demo-index) (length (unbox shuffled-demos)))
         (set-box! shuffled-demos (shuffle-list demos-list))
         (set-box! demo-index 0))
  (set-box! last-switch (current-inexact-milliseconds))
  (set-box! next-switch (+ (unbox last-switch) (+ 10000 (random 20000))))]
      [(or (char=? key #\g) (char=? key #\G))
       (set-box! bg-color (if (equal? (unbox bg-color) BLACK) GREEN-SCREEN BLACK))])))

(define (setup)
  (size WIDTH HEIGHT)
  (frame-rate FPS))
