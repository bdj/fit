#lang racket/base
(require web-server/servlet
         web-server/servlet-env
         web-server/formlets
         web-server/formlets/dyn-syntax
         racket/list
         "weights.rkt")

(define exercises (make-parameter '(("Squat" "Bench Press") ("Deadlift" "Military Press"))))

(define (max-formlet exercise)
  (formlet (label ,exercise
              ,(=> input-int max))
           (cons exercise max)))

; todo : Ask Jay about a #%#* form that would allow top level lists turn into forests
(define maxes-formlet
  (formlet* `(div ,@(for*/list ([day (exercises)]
                                [exercise day])
                      (=>* (max-formlet exercise) maxes)))
            (make-hash maxes)))

(define (max-form results-url)
  (response/xexpr
   `(html (head (title "fit")
                (link ([rel "stylesheet"] [type "text/css"] [href "/style.css"])))
          (body (form ([action ,results-url])
                      ,@(formlet-display maxes-formlet)
                      (input ([type "submit"])))))))

(define (plan-table day week maxes)
  (define a-plan (plan day week maxes (exercises)))
  `(table
    (tr (th "#") (th "Exercise") (th "Weights") (th "Reps") (th "Actual Reps"))
    ,@(for/list ([i (in-naturals 1)]
                 [row a-plan])
        `(tr (td ,(number->string i))
             (td ,(first row))
             (td ,(weights-format (third row)))
             (td ,(number->string (second row)))
             (td)))))

(define (cookie-lookup req key)
  (define cookies (request-cookies req))
  (define cookie (findf (λ (c) (string=? key (client-cookie-name c))) cookies))
  (and cookie (client-cookie-value cookie)))

(define username-formlet 
  (formlet (label "username"
                  ,(=> input-string username))
           username))

(define (username-form results-url)
  (response/xexpr
   `(html (head (title "fit-username")
                (link ([rel "stylesheet"] [type "text/css"] [href "/style.css"])))
          (body (form ([action ,results-url])
                      ,@(formlet-display username-formlet)
                      (input ([type "submit"])))))))

(define (get-username req)
  (define username (cookie-lookup req "username"))
  (or username 
      (let ()
        (define username (formlet-process username-formlet (send/suspend username-form)))
        (define main (url->string (request-uri req)))
        (printf "~a\n" main)
        (send/back
         (redirect-to main see-other
                      #:headers (list (cookie->header (make-cookie "username" username))))))))



(define (start req)
  (define username (get-username req))
  (define maxes (formlet-process maxes-formlet (send/suspend max-form)))
  (response/xexpr
   `(html (head (title "fit2")
                (link ([rel "stylesheet"] [type "text/css"] [href "/style.css"])))
          (body 
           ,@(for*/list ([week (in-range 4)]
                         [day (in-range (length (exercises)))])
               (plan-table day week maxes))))))

(serve/servlet start #:server-root-path ".")