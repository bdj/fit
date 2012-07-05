#lang racket/base
(require racket/list
         racket/string)
(provide plan weights-format)

(module+ test
  (require tests/eli-tester))

(module+ test
  (test
   (weights (* 315 .65)) =>
   '((45 1) (25 1) (5 1) (2.5 1))))

(define (mod/rem n m)
  (define mod (inexact->exact (floor (/ n m))))
  (values mod (- n (* m mod))))

(define (weights w [plates '(45 35 25 10 5 2.5)])
  (define-values (result rem)
    (for/fold ([result empty]
               [rem-weight (/ (- w 45) 2)])
      ([plate plates])
      (define-values (num rem) (mod/rem rem-weight plate))
      (if (zero? num)
          (values result rem)
          (values (cons (list plate num) result) rem))))
  (reverse result))

(module+ test
  (test
   (plan 0 0 #hash(("Squat" . 315) ("Bench Press" . 245))) =>
   '(("Squat" 5 ((45 1) (25 1) (5 1) (2.5 1)))
     ("Squat" 5 ((45 2) (5 1)))
     ("Squat" 5 ((45 2) (10 2)))
     ("Bench Press" 5 ((45 1) (10 1)))
     ("Bench Press" 5 ((45 1) (10 2) (2.5 1)))
     ("Bench Press" 5 ((45 1) (35 1))))))

(define (plan day week maxes 
                 [exercises '(("Squat" "Bench Press") ("Deadlift" "Military Press"))] 
                 [workouts '(((5 .65) (5 .75) (5 .85)) 
                             ((3 .7) (3 .8) (3 .9))
                             ((5 .75) (3 .85) (1 .95))
                             ((5 .4) (5 .5) (5 .6)))])
  (for*/list ([exercise (list-ref exercises day)]
              [workout (list-ref workouts week)])
    (list exercise (first workout) (weights (* (hash-ref maxes exercise) (second workout))))))

(define (weights-format weights)
  (string-join 
   (for/list ([weight weights])
     (format "~ax~a" (first weight) (second weight)))
   ", "))

