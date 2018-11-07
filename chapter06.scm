;; Chapter 6: The Fun Never Ends...

(load "test-check.scm")
(load "minikanren/mkprelude.scm")

;; Page 77
(define anyo
  (lambda (g)
    (conde
     (g succeed)
     (else (anyo g)))))

;; Page 77
(define nevero (anyo fail))

;; Page 77
(test "nevero"
      (run 1 (q)
        fail
        nevero)
      '())

;; Page 77
(define alwayso (anyo succeed))

;; Page 77
(test "alwayso"
      (run 1 (q)
        alwayso
        (== #t q))
      '(#t))

;; Page 78
(test "alwayso"
      (run 5 (q)
        alwayso
        (== #t q))
      '(#t #t #t #t #t))

;; Succeeds at least once
;; Page 78
(define salo
  (lambda (g)
    (conde
     (succeed succeed)
     (else g))))

;; Page 78
(test "salo"
      (run 1 (q)
        (salo alwayso)
        (== #t q))
      '(#t))

;; Page 78
(test "salo"
      (run 1 (q)
        (salo nevero)
        (== #t q))
      '(#t))

;; Page 80
(test "condi"
      (run 1 (q)
        (condi
         ((== #f q) alwayso)
         (else (== #t q)))
        (== #t q))
      '(#t))

;; Page 80
(test "condi"
      (run 5 (q)
        (condi
         ((== #f q) alwayso)
         (else (anyo (== #t q))))
        (== #t q))
      '(#t #t #t #t #t))

;; Page 14
(define teacupo
  (lambda (x)
    (conde
     ((== 'tea x) succeed)
     ((== 'cup x) succeed)
     (else fail))))

;; Page 14
(test "teacupo"
      (run* (x)
        (teacupo x))
      '(tea cup))

;; Page 14
(test "teacupo"
      (run* (r)
        (fresh (x y)
          (conde
           ((teacupo x) (== #t y) succeed)
           ((== #f x) (== #t y))
           (else fail))
          (== (cons x (cons y '())) r)))
      '((tea #t) (cup #t) (#f #t)))

;; Page 81
(test "condi"
      (run 5 (r)
        (condi
         ((teacupo r) succeed)
         ((== #f r) succeed)
         (else fail)))
      '(tea #f cup))

;; Why would conde return no value here?
;; 
;; Because if the first conde line '(== #f q)' succeeds, then '(== #t
;; q)' fails. Now conde would evaluate 'alwayso' in the first conde
;; line '((== #f q) alwayse)' which won't stop.
;; 
;; Page 81
(test "condi"
      (run 5 (q)
        (condi
         ((== #f q) alwayso)
         ((== #t q) alwayso)
         (else fail))
        (== #t q))
      '(#t #t #t #t #t))

;; Why would condi return no value here?
;;
;; Because after the first 'run' when 'alwayso' in the first condi
;; line succeeds. condi goes to its second line to 'nevero' and thus
;; returns no value.
;;
;; Page 82
(test "conde"
      (run 5 (q)
        (conde
         (alwayso succeed)
         (else nevero))
        (== #t q))
      '(#t #t #t #t #t))

;; This has no value
;; Page 82
;; (test "conde"
;;       (run 1 (q)
;;         (all
;;          (conde
;;           ((== #f q) succeed)
;;           (else (== #t q)))
;;          alwayso)
;;         (== #t q))
;;       '())

;; Page 83
(test "alli"
      (run 1 (q)
        (alli
         (conde
          ((== #f q) succeed)
          (else (== #t q)))
         alwayso)
        (== #t q))
      '(#t))

;; Page 83
(test "alli"
      (run 5 (q)
        (alli
         (conde
          ((== #f q) succeed)
          (else (== #t q)))
         alwayso)
        (== #t q))
      '(#t #t #t #t #t))

;; Page 83
(test "alli"
      (run 5 (q)
        (alli
         (conde
          ((== #t q) succeed)
          (else (== #f q)))
         alwayso)
        (== #t q))
      '(#t #t #t #t #t))

;; Why does the same expression return no value with alli instead of all?
;;
;; Because after the first run, after the first conde line succeeds,
;; alli moves to the second conde line and nevero never succeeds, so
;; no value.  With all instead of alli the second conde line is never
;; reached and nevero never evaluated.
;; 
;; Page 84
(test "alli"
      (run 5 (q)
        (all
         (conde
          (succeed succeed)
          (else nevero))
         alwayso)
        (== #t q))
      '(#t #t #t #t #t))
