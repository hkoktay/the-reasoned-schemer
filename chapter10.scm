;; Chapter 10: Thin Ice

(load "test-check.scm")
(load "minikanren/mkprelude.scm")

;; Page 145
(test "conda"
      (run* (q)
        (conda
         (fail succeed)
         (else fail))
        (== q #t))
      '())

;; Page 145
(test "conda"
      (run* (q)
        (conda
         (fail succeed)
         (else succeed))
        (== q #t))
      '(#t))

;; Page 145
(test "conda"
      (run* (q)
        (conda
         (succeed fail)
         (else succeed))
        (== q #t))
      '())

;; Page 145
(test "conda"
      (run* (q)
        (conda
         (succeed succeed)
         (else fail))
        (== q #t))
      '(#t))

;; Page 145
(test "conda"
      (run* (x)
        (conda
         ((== 'olive x) succeed)
         ((== 'oil x) succeed)
         (else fail)))
      '(olive))

;; Page 146
(test "conda"
      (run* (x)
        (conda
         ((== 'virgin x) fail)
         ((== 'olive x) succeed)
         ((== 'oil x) succeed)
         (else fail)))
      '())

;; Page  146
(test "conda"
      (run* (q)
        (fresh (x y)
          (== 'split x)
          (== 'pea y)
          (conda
           ((== x y) (== 'split x))
           (else succeed)))
        (== #t q))
      '(#t))

;; not-pastao: [s-exp] -> [goal]
;; Page 147
(define not-pastao
  (lambda (x)
    (conda
     ((== 'pasta x) fail)
     (else succeed))))

;; Page 147
(test "not-pastao"
      (run* (x)
        (conda
         ((not-pastao x) fail)
         (else (== 'spaghetti x))))
      '(spaghetti))

;; Page 147
(test "not-pastao"
      (run* (x)
        (conda
         ((not-pastao x) fail)
         (else (== 'spaghetti x))))
      '(spaghetti))

;; Page 147
(test "not-pastao"
      (run* (x)
        (== 'spaghetti x)
        (conda
         ((not-pastao x) fail)
         (else (== 'spaghetti x))))
      '())

;; Has no value since alwayso never finishes
;; Page 148
;; (test "alwayso"
;;       (run* (q)
;;         (conda
;;          (alwayso succeed)
;;          (else fail))
;;         (== #t q))
;;       '(...))

;; Page 148
(test "condu"
      (run* (q)
        (condu
         (alwayso succeed)
         (else fail))
        (== #t q))
      '(#t))

;; Has no value since alwayso never finishes
;; Page 148
;; (test "alwayso"
;;       (run* (q)
;;         (condu
;;          (succeed alwayso)
;;          (else fail))
;;         (== #t q))
;;       '(...))

;; Has no value
;; Page 149
;; (test "conda"
;;       (run 1 (q)
;;         (conda
;;          (alwayso succeed)
;;          (else fail))
;;         fail
;;         (== #t q))
;;       '(...))

;; Page 149
(test "conda"
      (run 1 (q)
        (condu
         (alwayso succeed)
         (else fail))
        fail
        (== #t q))
      '())

;; onceo: [goal] -> [goal]
;; Page 149
(define onceo
  (lambda (g)
    (condu
     (g succeed)
     (else fail))))

;; teacupo: any -> [atom]
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

;; Page 149
(test "onceo"
      (run* (x)
        (onceo (teacupo x)))
      '(tea))

;; salo: any -> [goal]
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

;; Page 150
(test "onceo"
      (run 1 (g)
        (onceo (salo nevero))
        fail)
      '())

;; Page 150
(test "teacupo with conde"
      (run* (r)
        (conde
         ((teacupo r) succeed)
         ((== #f r) succeed)
         (else fail)))
      '(tea cup #f))

;; Page 150
(test "teacupo with conda"
      (run* (r)
        (conda
         ((teacupo r) succeed)
         ((== #f r) succeed)
         (else fail)))
      '(tea cup))

;; Page 150
(test "teacupo with conda"
      (run* (r)
        (== #f r)
        (conda
         ((teacupo r) succeed)
         ((== #f r) succeed)
         (else fail)))
      '(#f))

;; Page 150
(test "teacupo with condu"
      (run* (r)
        (== #f r)
        (condu
         ((teacupo r) succeed)
         ((== #f r) succeed)
         (else fail)))
      '(#f))

;; bumpo: [bl] [bl] -> [goal]
;; Page 151
(define bumpo
  (lambda (n x)
    (conde
     ((== n x) succeed)
     (else
      (fresh (m)
        (-o n '(1) m)
        (bumpo m x))))))

;; Page 151
(test "bumpo"
      (run* (x)
        (bumpo '(1 1 1) x))
      '((1 1 1)
        (0 1 1)
        (1 0 1)
        (0 0 1)
        (1 1)
        (0 1)
        (1)
        ()))

;; gen&testo: fun [bl] [bl] [bl] -> [goal]
;; Page 151
(define gen&testo
  (lambda (op i j k)
    (onceo
     (fresh (x y z)
       (op x y z)
       (== i x)
       (== j y)
       (== k z)))))

;; Page 151
(test "gen&testo"
      (run* (q)
        (gen&testo +o '(0 0 1) '(1 1) '(1 1 1))
        (== #t q))
      '(#t))

;; Has no value
;; Page 153
;; (test "gen&testo"
;;       (run 1 (g)
;;         (gen&testo +o '(0 0 1) '(1 1) '(0 1 1)))
;;       '(...))

;; enumerateo: fun [bl] [bl] -> [goal]
;; Page 154
(define enumerateo
  (lambda (op r n)
    (fresh (i j k)
      (bumpo n i)
      (bumpo n j)
      (op i j k)
      (gen&testo op i j k)
      (== `(,i ,j ,k) r))))

;; Page 154
(test "enumerateo"
      (run* (s)
        (enumerateo +o s '(1 1)))
      '(((1 1) (1 1) (0 1 1))
        ((1 1) (0 1) (1 0 1))
        ((1 1) (1) (0 0 1))
        ((1 1) () (1 1))
        ((0 1) (1 1) (1 0 1))
        ((0 1) (0 1) (0 0 1))
        ((0 1) (1) (1 1))
        ((0 1) () (0 1))
        ((1) (1 1) (0 0 1))
        ((1) (0 1) (1 1))
        ((1) (1) (0 1))
        ((1) () (1))
        (() (1 1) (1 1))
        (() (0 1) (0 1))
        (() (1) (1))
        (() () ())))

;; Page 156
(test "enumerateo"
      (run 1 (s)
        (enumerateo +o s '(1 1 1)))
      '(((1 1 1) (1 1 1) (0 1 1 1))))

;; gen-addero: [bl] [bl] [bl] [bl] -> [goal]
;; Page 156
(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
      (== `(,a . ,x) n)
      (== `(,b . ,y) m) (poso y)
      (== `(,c . ,z) r) (poso z)
      (all
       (full-addero d a b c e)
       (addero e x y z)))))

;; Has no value
;; Page 156
;; (test "gen&testo"
;;       (run 1 (q)
;;         (gen&testo +o '(0 1) '(1 1) '(1 0 1)))
;;       '(...))

;; Has no value
;; Page 156
;; (test "enumerateo"
;;       (run* (q)
;;         (enumerateo +o q '(1 1 1)))
;;       '(...))

(test "addero"
      (run 22 (s)
        (fresh (x y r)
          (addero 0 x y r)
          (== `(,x ,y ,r) s)))
      '((_.0 () _.0)
        (() (_.0 . _.1) (_.0 . _.1))
        ((1) (1) (0 1))
        ((1) (0 _.0 . _.1) (1 _.0 . _.1))
        ((0 _.0 . _.1) (1) (1 _.0 . _.1))
        ((1) (1 1) (0 0 1))
        ((0 1) (0 1) (0 0 1))
        ((1) (1 0 _.0 . _.1) (0 1 _.0 . _.1))
        ((1 1) (1) (0 0 1))
        ((1) (1 1 1) (0 0 0 1))
        ((0 1) (0 0 _.0 . _.1) (0 1 _.0 . _.1))
        ((1) (1 1 0 _.0 . _.1) (0 0 1 _.0 . _.1))
        ((1 0 _.0 . _.1) (1) (0 1 _.0 . _.1))
        ((1) (1 1 1 1) (0 0 0 0 1))
        ((0 0 _.0 . _.1) (0 1) (0 1 _.0 . _.1))
        ((1) (1 1 1 0 _.0 . _.1) (0 0 0 1 _.0 . _.1))
        ((1 1 1) (1) (0 0 0 1))
        ((1) (1 1 1 1 1) (0 0 0 0 0 1))
        ((0 1) (0 1 1) (0 0 0 1))
        ((1) (1 1 1 1 0 _.0 . _.1) (0 0 0 0 1 _.0 . _.1))
        ((1 1 0 _.0 . _.1) (1) (0 0 1 _.0 . _.1))
        ((1) (1 1 1 1 1 1) (0 0 0 0 0 0 1))))

;; Ground values with the gen-addero of chapter 10 (above)
;; (((1) (1) (0 1))
;;  ((1) (1 1) (0 0 1))
;;  ((0 1) (0 1) (0 0 1))
;;  ((1 1) (1) (0 0 1))
;;  ((1) (1 1 1) (0 0 0 1))
;;  ((1) (1 1 1 1) (0 0 0 0 1))
;;  ((1 1 1) (1) (0 0 0 1))
;;  ((1) (1 1 1 1 1) (0 0 0 0 0 1))
;;  ((0 1) (0 1 1) (0 0 0 1))
;;  ((1) (1 1 1 1 1 1) (0 0 0 0 0 0 1)))

;; Ground values with the gen-addero of chapter 7
;; Notice the missing values.
;; (((1) (1) (0 1))
;;  ((1) (1 1) (0 0 1))
;;  ((0 1) (0 1) (0 0 1))
;;  ((1 1) (1) (0 0 1))
;;  ((1) (1 1 1) (0 0 0 1))
;;  ((1 1) (0 1) (1 0 1))
;;  ((1) (1 1 1 1) (0 0 0 0 1))
;;  ((1 1 1) (1) (0 0 0 1))
;;  ((1) (1 1 1 1 1) (0 0 0 0 0 1))
;;  ((0 1) (1 1) (1 0 1))
;;  ((1) (1 1 1 1 1 1) (0 0 0 0 0 0 1)))
