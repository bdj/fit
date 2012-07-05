#lang racket/base
(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/dyn-syntax
         racket/list
         "weights.rkt")

(define exercises '(("Squat" "Bench Press") ("Deadlift" "Military Press")))

(define (max-formlet exercise)
  (formlet (p ,exercise
              ,(=> input-int max))
           (cons exercise max)))

; todo : Ask Jay about a #%#* form that would allow top level lists turn into forests
(define (maxes-formlet exercises)
  (formlet* `(div ,@(for*/list ([day exercises]
                                [exercise day])
                      (=>* (max-formlet exercise) maxes)))
            (make-hash maxes)))


(define (start req)
  (define new-req
    (send/suspend
     (λ (url)
       (response/xexpr
        `(html (head (title "fit"))
               (body (form ([action ,url])
                           ,@(formlet-display (maxes-formlet exercises))
                           (input ([type "submit"])))))))))
  (define maxes (formlet-process (maxes-formlet exercises) new-req))
  (response/xexpr
   `(html (head (title "fit2"))
          (body 
           ,@(for*/list ([week (in-range 4)]
                         [day (in-range (length exercises))])
               (define a-plan (plan day week maxes exercises))
               `(table
                 (tr (th "#") (th "Exercise") (th "Weights") (th "Reps") (th "Actual Reps"))
                 ,@(for/list ([i (in-naturals 1)]
                              [row a-plan])
                     `(tr (td ,(number->string i))
                          (td ,(first row))
                          (td ,(weights-format (third row)))
                          (td ,(number->string (second row)))
                          (td)))))))))

(serve/servlet start)