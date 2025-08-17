#!/usr/bin/env racket
#lang sketching
(provide shuffle-list demos-list single-demo demo-index shuffled-demos draw demo-hold? bg-color ORANGE set-box! unbox)
(provide demo!set demo!shuffle demo!skip demo!prev demo!hold demo!list demo!next  demo!script)

(require "demos/demos.rkt")
(require racket/cmdline)
(require racket/string)


(define WIDTH 1920)
(define HEIGHT 1080)
(define FPS 24)

(define ORANGE (color 255 140 0))
(define WHITE 255)
(define BLACK (color 0 0 0))
(define GREEN-SCREEN (color 0 255 0))
(define bg-color (box BLACK))


(define demos-list (list 'globe 'cube 'waves 'lissajous 'starfield 'spiral 'tunnel 'metaballs 'smiley 'lambdas 'pipes 'boids 'snowing 'dvd 'ripple))
(define single-demo (box #f))


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
(define demo-hold? (box #f))



(define (draw)
  (define now (current-inexact-milliseconds))
  (define current-demo
    (if (unbox single-demo)
        (unbox single-demo)
        (begin
          (when (and (not (unbox demo-hold?)) (> now (unbox next-switch)))
            (set-box! demo-index (+ (unbox demo-index) 1))
            (when (>= (unbox demo-index) (length (unbox shuffled-demos)))
              (set-box! demo-index 0))
            (set-box! last-switch now)
            (set-box! next-switch (+ now (+ 10000 (random 20000)))) )
          (list-ref (unbox shuffled-demos) (unbox demo-index)))))
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

;; --- Demo! REPL commands for use in xrepl ---

;; (demo!list) : Show all available demos
(define (demo!list)
  (displayln (string-append "Available demos: " (string-join (map symbol->string demos-list) ", "))))

;; (demo!skip) : Go forward in shuffle
(define (demo!skip)
  (set-box! demo-index (+ (unbox demo-index) 1))
  (when (>= (unbox demo-index) (length (unbox shuffled-demos)))
    (set-box! demo-index 0)
    (if (<= (length (unbox shuffled-demos)) 1)
        (displayln "No more demos in shuffle. Add more or use #:regen #t to repopulate.")
        (void)))
  (set-box! last-switch (current-inexact-milliseconds))
  (set-box! next-switch (+ (unbox last-switch) (+ 10000 (random 20000))))
  (set-box! last-demo #f)
  (displayln "Skipped to next demo."))

(define (demo!next)
  (demo!skip))

;; (demo!prev) : Go backward in shuffle
(define (demo!prev)
  (set-box! demo-index (- (unbox demo-index) 1))
  (when (< (unbox demo-index) 0)
    (set-box! demo-index (- (length (unbox shuffled-demos)) 1))
    (if (<= (length (unbox shuffled-demos)) 1)
        (displayln "No more demos in shuffle. Add more or use #:regen #t to repopulate.")
        (void)))
  (set-box! last-switch (current-inexact-milliseconds))
  (set-box! next-switch (+ (unbox last-switch) (+ 10000 (random 20000))))
  (set-box! last-demo #f)
  (displayln "Went to previous demo."))

;; (demo!hold) : Toggle hold mode

;; (demo!hold) : Toggle hold mode
;; (demo!hold #:on #t) : Set hold ON
;; (demo!hold #:on #f) : Set hold OFF
(define (demo!hold #:on [on-val 'not-supplied])
  (if (eq? on-val 'not-supplied)
      (begin
        (set-box! demo-hold? (not (unbox demo-hold?)))
        (displayln (string-append "Demo hold is now " (if (unbox demo-hold?) "ON" "OFF"))))
      (begin
        (set-box! demo-hold? on-val)
        (displayln (string-append "Demo hold is now " (if (unbox demo-hold?) "ON" "OFF"))))))




(define (set-demo-by-symbol sym)
  (when (member sym demos-list)
    (set-box! single-demo sym)
    (set-box! last-demo #f)
    (displayln (string-append "Switched to demo: " (symbol->string sym)))
    #t))


(define (set-auto-mode)
  (set-box! single-demo #f)
  (set-box! demo-index 0)
  (set-box! last-demo #f)
  (displayln "Switched to auto shuffle mode."))

;; --- Demo! REPL commands for shuffle navigation and hold ---

;; (demo!shuffle #:add '(demo1 demo2) #:rm '(demo3) #:list #t #:set '(demo4 ...) #:flush #t #:regen #t)
(define (demo!shuffle #:add [add-list '()] #:rm [rm-list '()] #:list [show-list #f] #:set [set-list #f] #:flush [flush #f] #:regen [regen #f])
  (when set-list
    (set-box! shuffled-demos (shuffle-list set-list))
    (set-box! demo-index 0)
    (displayln (string-append "Shuffle list set to: " (string-join (map symbol->string (unbox shuffled-demos)) ", "))))
  (when (not (null? add-list))
    (for ([sym add-list])
      (cond
        [(not (member sym demos-list))
         (displayln (string-append "Cannot add: Unknown demo '" (symbol->string sym) "'."))]
        [(member sym (unbox shuffled-demos))
         (displayln (string-append "Demo '" (symbol->string sym) "' is already in shuffle list."))]
        [else
         (set-box! shuffled-demos (append (unbox shuffled-demos) (list sym)))
         (displayln (string-append "Added demo '" (symbol->string sym) "' to shuffle list."))])))
  (when (not (null? rm-list))
    (for ([sym rm-list])
      (if (member sym (unbox shuffled-demos))
          (begin
            (set-box! shuffled-demos (filter (lambda (x) (not (equal? x sym))) (unbox shuffled-demos)))
            (displayln (string-append "Removed demo '" (symbol->string sym) "' from shuffle list.")))
          (displayln (string-append "Demo '" (symbol->string sym) "' is not in shuffle list.")))))
  (when flush
    (let* ([sd (unbox shuffled-demos)]
           [idx (unbox demo-index)]
           [active (if (and (pair? sd) (<= 0 idx) (< idx (length sd)))
                       (list-ref sd idx)
                       (car sd))])
      (set-box! shuffled-demos (list active))
      (set-box! demo-index 0)
      (displayln (string-append "Shuffle list flushed to only: " (symbol->string active)))))
  (when regen
    (set-box! shuffled-demos (shuffle-list demos-list))
    (set-box! demo-index 0)
    (displayln "Shuffle list regenerated from all demos."))
  (when show-list
    (if (unbox demo-hold?)
        (displayln "Cannot display shuffle list: demo hold is ON.")
        (displayln (string-append "Shuffle list: " (string-join (map symbol->string (unbox shuffled-demos)) ", ")))))
  (void))

;; (demo!set 'waves) or (demo!set #f) for auto

;; (demo!set 'auto) : disables hold and starts shuffle
(define (demo!set sym)
  (cond
  [(or (eq? sym #f) (eq? sym 'auto))
   (demo!hold #:on #f)
   (set-auto-mode)]
    [(set-demo-by-symbol sym) (void)]
    [else (displayln (string-append "Unknown demo: " (format "~a" sym)))]))


;; (demo!script (list (lambda () (demo!set 'waves)) 3 (lambda () (demo!set 'spiral)) 2 ...))
;; Each number is a delay in seconds before the next action.
(define (demo!script actions)
  (thread
   (lambda ()
     (for ([a actions])
       (cond
         [(procedure? a) (a)]
         [(number? a) (sleep a)]
         [else (displayln (format "Unknown script action: ~a" a))])))))

;;
;; Examples for demo!script usage:
;;
;; Switch to 'waves, wait 2s, then 'spiral, wait 2s, then auto shuffle, then hold:
;;
;;   (demo!script
;;     (list
;;       (lambda () (demo!set 'waves)) 2
;;       (lambda () (demo!set 'spiral)) 2
;;       (lambda () (demo!set #f)) 2
;;       (lambda () (demo!hold))))
;;
;; Cycle through several demos with 1s delay each:
;;
;;   (demo!script
;;     (list
;;       (lambda () (demo!set 'cube)) 1
;;       (lambda () (demo!set 'pipes)) 1
;;       (lambda () (demo!set 'lissajous)) 1
;;       (lambda () (demo!set 'starfield))))
;;
;; Toggle hold on, wait 3s, then off:
;;
;;   (demo!script
;;     (list
;;       (lambda () (demo!hold)) 3
;;       (lambda () (demo!hold))))
;;



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
       (set-box! bg-color (if (equal? (unbox bg-color) BLACK) GREEN-SCREEN BLACK))]
  [(char=? key #\1)
   (demo!prev)]
  [(char=? key #\2)
   (demo!next)]
  [(or (char=? key #\h) (char=? key #\H))
   (demo!hold)])))


(define (setup)
  (size WIDTH HEIGHT)
  (frame-rate FPS))

;; Entry point

;; Only run main if this file is run as a script, not when required
(module+ main
  (define demo-name #f)
  (command-line
    #:program "demostream"
    #:once-each
    [("--demo") name "Run a specific demo by name" (set! demo-name name)])
  (when demo-name
    (define sym (string->symbol demo-name))
    (if (member sym demos-list)
        (begin (set-box! single-demo sym)
               (set-box! last-demo #f))
        (begin (displayln (string-append "Unknown demo: " demo-name))
               (exit 1))))
  (void))

