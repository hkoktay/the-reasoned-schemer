;; Chapter 1: Playthings

(load "test-check.scm")
(load "minikanren/mkprelude.scm")

;; 'succeed' always returns its argument
(test "#s" (succeed 'ok) 'ok)
(test "#s" (succeed 'foo) 'foo)
(test "#s" (succeed 'lambda) 'lambda)

;; 'fail' always returns #f
(test "#u" (fail 'ok) #f)
(test "#u" (fail 'foo) #f)
(test "#u" (fail 'lambda) #f)

(test "run*"
      (run* (q)
        fail)
      '())

(test "run*"
      (run* (q)
        (== #t q))
      '(#t))

(test "run*"
      (run* (q)
        fail
        (== #t q))
      '())

(test "run*"
      (run* (q)
        succeed
        (== #t q))
      '(#t))

;; The value 'corn is associated with the variable 'r', thus the value
;; is '(corn)
(test "run*"
      (run* (r)
        succeed
        (== 'corn r))
      '(corn))

;; The value is '() because run returns '() if any of its goals fails
;; and 'fail' always fails.
(test "run*"
      (run* (r)
        fail
        (== 'corn r))
      '())

(test "run*"
      (run* (q)
        succeed
        (== #f q))
      '(#f))

(test "run* with let"
      (run* (x)
        (let ((x #f))
          (== #t x)))
      '())

(test "=="
      (run* (q)
        (fresh (x)
         (== #t x)
         (== #t q)))
      '(#t))

;; x and q start as fresh variables
;; The Law of Fresh
(test "=="
      (run* (q)
        (fresh (x)
         (== #t x)
         (== #t q)))
      '(#t))

;; The Law of ==
(test "=="
      (run* (q)
        (fresh (x)
          (== x #t)
          (== #t q)))
      '(#t))

(test "=="
      (run* (q)
        (fresh (x)
          (== x #t)
          (== q #t)))
      '(#t))

;; The symbol '_.0' represents a fresh variable. The variable could be
;; anything.
(test "fresh variable"
      (run* (x) succeed)
      '(_.0))

;; Page 8
(test "scope of fresh"
      (run* (x)
        (let ((x #f))
          (fresh (x)
            (== #t x))))
      '(_.0))

;; Page 8
(test "fresh cons pair"
      (run* (r)
        (fresh (x y)
          (== (cons x (cons y '())) r)))
      '((_.0 _.1)))

;; Page 8
(test "fresh cons pair"
      (run* (s)
        (fresh (t u)
          (== (cons t (cons u '())) s)))
      '((_.0 _.1)))

;; Page 8
(test "let with list"
      (run* (r)
        (fresh (x)
          (let ((y x))
            (fresh (x)
              (== (cons y (cons x (cons y '()))) r)))))
      '((_.0 _.1 _.0)))

;; Page 8
(test "let with list"
      (run* (r)
        (fresh (x)
          (let ((y x))
            (fresh (x)
              (== (cons x (cons y (cons x '()))) r)))))
      '((_.0 _.1 _.0)))

;; Page 9
(test "run*"
      (run* (q)
        (== #f q)
        (== #t q))
      '())

;; Page 9
(test "run*"
      (run* (q)
        ;; q gets associated with #f
        (== #f q)
        (== #f q))
      '(#f))

;; Page 9
(test "run*"
      (run* (q)
        (let ((x q))
          (== #t q)))
      '(#t))

;; Page 9
(test "co-refering variables"
      (run* (r)
        (fresh (x)
          (== x r)))
      '(_.0))

;; Page 9
(test "variable association"
      (run* (q)
        (fresh (x)
          (== #t x)
          (== x q)))
      '(#t))

;; Page 9
(test "variable association"
      (run* (q)
        (fresh (x)
          (== x q)
          (== #t x)))
      '(#t))

;; Page 10
(test "variable association"
      (run* (q)
        (fresh (x)
          (== #t x)
          (== x q)))
      '(#t))

;; Page 10
(test "eq? variables"
      (run* (q)
        (fresh (x)
          (== (eq? x q) q)))
      '(#f))

;; Page 10
(test "eq? variables"
      (run* (q)
        (let ((x q))
         (fresh (q)
           (== (eq? x q) x))))
      '(#f))

;; First conde line succeeds therefore the answer is 'succeed'. To get
;; the second value of the conde line we pretend that (== 'olive x)
;; fails (unlike 'cond'); this refreshes the variable 'x'. Now (==
;; 'oil x) succeeds. Then again we pretend that this conde line fails
;; until no conde line remains. The outcome is two values 'olive and
;; 'oil, because the 'succeed' preserves the values of the conde
;; lines.
;; Page 11
(test "The Law of Conde"
      (run* (x)
        (conde
         ((== 'olive x) succeed)
         ((== 'oil x) succeed)
         (else fail)))
      '(olive oil))

;; Page 12
(test "conde with run 1"
      (run 1 (x)
        (conde
         ((== 'olive x) succeed)
         ((== 'oil x) succeed)
         (else fail)))
      '(olive))

;; The second value of the result is _.0 because this conde line
;; succeeds without 'x' getting an association.
;; Page 12
(test "conde with fail line"
      (run* (x)
        (conde
         ((== 'virgin x) fail)
         ((== 'olive x) succeed)
         (succeed succeed)
         ((== 'oil x) succeed)
         (else fail)))
      '(olive _.0 oil))

;; Page 12
(test "conde"
      (run* (x)
        (conde
         ((== 'olive x) succeed)
         (succeed succeed)
         ((== 'oil x) succeed)
         (else fail)))
      '(olive _.0 oil))

;; Page 13
(test "conde with run 2"
      (run 2 (x)
        (conde
         ((== 'extra x) succeed)
         ((== 'virgin x) fail)
         ((== 'olive x) succeed)
         ((== 'oil x) succeed)
         (else fail)))
      '(extra olive))

(test "same conde with run*"
      (run* (x)
        (conde
         ((== 'extra x) succeed)
         ((== 'virgin x) fail)
         ((== 'olive x) succeed)
         ((== 'oil x) succeed)
         (else fail)))
      '(extra olive oil))

;; Page 13
(test "fresh with multiple values"
      (run* (r)
        (fresh (x y)
          (== 'split x)
          (== 'pea y)
          (== (cons x (cons y '())) r)))
      '((split pea)))

;; Page 13
(test "conde with multiple values"
      (run* (r)
        (fresh (x y)
          (conde
           ((== 'split x) (== 'pea y))
           ((== 'navy x) (== 'bean y))
           (else fail))
          (== (cons x (cons y '())) r)))
      '((split pea) (navy bean)))

;; Page 13
(test "conde evaluation order"
      (run* (r)
        (fresh (x y)
          (conde
           ((== 'split x) (== 'pea y))
           ((== 'navy x) (== 'bean y))
           (else fail))
          (== (cons x (cons y (cons 'soup '()))) r)))
      '((split pea soup) (navy bean soup)))

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

;; Page 14
(test "with fresh variables only"
      (run* (r)
        (fresh (x y z)
          (conde
           ((== y x) (fresh (x) (== z x)))
           ((fresh (x) (== y x)) (== z x))
           (else fail))
          (== (cons y (cons z '())) r)))
      '((_.0 _.1) (_.0 _.1)))

;; Page 15
(test "with fresh variables only"
      (run* (r)
        (fresh (x y z)
          (conde
           ((== y x) (fresh (x) (== z x)))
           ((fresh (x) (== y x)) (== z x))
           (else fail))
          (== #f x)
          (== (cons y (cons z '())) r)))
      '((#f _.0) (_.0 #f)))

;; Page 15
(test "let"
      (run* (q)
        (let ((a (== #t q))
              (b (== #f q)))
          b))
      '(#f))

;; Page 15
(test "let"
      (run* (q)
        (let ((a (== #t q))
              (b (fresh (x)
                   (== x q)
                   (== #f x)))
              (c (conde
                  ((== #t q) succeed)
                  (else (== #f q)))))
          b))
      '(#f))
