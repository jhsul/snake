;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH 600)
(define HEIGHT WIDTH)

;; 20x20 grid, each square is 30x30

(define GRID-WIDTH 20)

(define MTS (rectangle WIDTH HEIGHT "solid" "black"))

(define SQUARE-WIDTH (/ WIDTH GRID-WIDTH))

(define WHITE-SQUARE (square SQUARE-WIDTH "solid" "white"))
(define RED-SQUARE (square SQUARE-WIDTH "solid" "red"))

(define-struct pos (x y))
(define-struct world-state (direction ticks lop target))

(define START (make-world-state "down" 0 (list (make-pos 1 1))
                                (make-pos (random 20) (random 20))))
(define WS1 (make-world-state "left" 0 (list (make-pos 3 3)
                                             (make-pos 2 3))
                              (make-pos 10 10)))
(define (main ws)
  (big-bang ws
    (on-tick next-tick)
    (to-draw render-ws)
    (on-key change-direction)))

(define (next-tick ws)
  (cond
    [(zero? (modulo (world-state-ticks ws) 7))
     (next-ws ws)]
    [else
     (make-world-state
      (world-state-direction ws)
      (add1 (world-state-ticks ws))
      (world-state-lop ws)
      (world-state-target ws))])) 

(define (next-ws ws)
  (local [(define hit-border
            (or (< (pos-x (first (world-state-lop ws))) 0)
                (>= (pos-x (first (world-state-lop ws))) GRID-WIDTH)
                (< (pos-y (first (world-state-lop ws))) 0)
                (>= (pos-y (first (world-state-lop ws))) GRID-WIDTH)))
          (define (remove-last l) (reverse (rest (reverse l))))
          (define (next-pos ws)
            (make-pos
             (cond [(string=? (world-state-direction ws) "left")
                    (sub1 (pos-x (first (world-state-lop ws))))]
                   [(string=? (world-state-direction ws) "right")
                    (add1 (pos-x (first (world-state-lop ws))))]
                   [else
                    (pos-x (first (world-state-lop ws)))])
             (cond [(string=? (world-state-direction ws) "up")
                    (sub1 (pos-y (first (world-state-lop ws))))]
                   [(string=? (world-state-direction ws) "down")
                    (add1 (pos-y (first (world-state-lop ws))))]
                   [else
                    (pos-y (first (world-state-lop ws)))])))
          (define hit-target (equal? (first (world-state-lop ws))
                                     (world-state-target ws)))
          (define hit-self
            (ormap
             (lambda (p) (equal? (first (world-state-lop ws)) p))
             (rest (world-state-lop ws))))]
    (if (or hit-border
            hit-self)
        START
        (make-world-state
         (world-state-direction ws)
         (add1 (world-state-ticks ws))
         (cons (next-pos ws)
               (if hit-target
                   (world-state-lop ws)
                   (remove-last (world-state-lop ws))))
         (if hit-target
             (make-pos (random GRID-WIDTH) (random GRID-WIDTH))
             (world-state-target ws))))))
     
(define (render-ws ws)
  (local [(define (draw-pos p bg img)
            (place-image/align
             img
             (* (pos-x p) SQUARE-WIDTH)
             (* (pos-y p) SQUARE-WIDTH)
             "left" "top"
             bg))]

    (foldl (lambda (p bg) (draw-pos p bg WHITE-SQUARE))
           (draw-pos (world-state-target ws)
                     MTS
                     RED-SQUARE)
           (world-state-lop ws))))

(define (change-direction ws key)
  (make-world-state
   (cond
     [(key=? key "up") "up"]
     [(key=? key "down") "down"]
     [(key=? key "left") "left"]
     [(key=? key "right") "right"]
     [else
      (world-state-direction ws)])
   (world-state-ticks ws)
   (world-state-lop ws)
   (world-state-target ws)))



(main START)
