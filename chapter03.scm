;; Chapter 3: Seeing Old Friends in New Ways

(load "test-check.scm")
(load "minikanren/mkprelude.scm")

;; Page 27
(define list?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((pair? l) (list? (cdr l)))
     (else #f))))

(test "list?" (list? '((a) (b) (c))) #t)
(test "list?" (list? '()) #t)
(test "list?" (list? '(d a t e . s)) #f)

;; Page 27
(define listo
  (lambda (l)
    (conde
     ((nullo l) succeed)
     ((pairo l)
      (fresh (d)
        (cdro l d)
        (listo d)))
     (else fail))))

;; Page 28
(test "listo"
      (run* (x)
        (listo `(a b ,x d)))
      '(_.0))

;; Reasoning:
;; The list `(a b c . ,x) is a proper list when ,x is '(). So when
;; listo reaches the end of the list (nullo x) succeeds and associates
;; ,x with the empty list.
;; 
;; Page 29
(test "listo"
      (run 1 (x)
        (listo `(a b c . ,x)))
      '(()))

;; Page 29
(test "listo"
      (run 5 (x)
        (listo `(a b c . ,x)))
      '(()
        (_.0)
        (_.0 _.1)
        (_.0 _.1 _.2)
        (_.0 _.1 _.2 _.3)))

;; Page 30
(define lol?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((list? (car l)) (lol? (cdr l)))
     (else #f))))


;; Page 30
(define lolo
  (lambda (l)
    (conde
     ((nullo l) succeed)
     ((fresh (a)
        (caro l a)
        (listo a))
      (fresh (d)
        (cdro l d)
        (lolo d)))
     (else fail))))

;; Page 30
(test "lolo"
      (run 1 (l)
        (lolo l))
      '(()))

;; Page 31
(test "lolo"
      (run* (q)
        (fresh (x y)
          (lolo `((a b) (,x c) (d ,y)))
          (== #t q)))
      '(#t))

;; Page 31
(test "lolo"
      (run 1 (q)
        (fresh (x)
          (lolo `((a b) . ,x))
          (== #t q)))
      '(#t))

;; Page 31
(test "lolo"
      (run 1 (x)
        (lolo `((a b) (c d) . ,x)))
      '(()))

;; All values are possible solutions. In other words any of the
;; values `((a b) (c d) . ,x) is a proper list. For example '((a b) (c d) . (() ()))
;; Page 31
(test "lolo"
      (run 5 (x)
        (lolo `((a b) (c d) . ,x)))
      '(()
        (())
        (() ())
        (() () ())
        (() () () ())))

;; Page 32
(define twinso
  (lambda (s)
    (fresh (x y)
      (conso x y s)
      (conso x '() y))))

;; Page 32
(test "twinso"
      (run* (q)
        (twinso '(tofu tofu))
        (== #t q))
      '(#t))

;; Reasoning:
;; (twinso `(,z tofu))
;; (conso x y `(,z tofu))
;;    -> x := ,z
;;    -> y := (tofu)
;; (conso x '() '(tofu))
;;    -> x := tofu
;; So z := tofu and the result is tofu.
;; Page 32
(test "twinso"
      (run* (z)
        (twinso `(,z tofu)))
      '(tofu))

;; 1.revision
(define twinso
  (lambda (s)
    (fresh (x)
      (== `(,x ,x) s))))

;; Page 32
(test "twinso - 1.revision"
      (run* (q)
        (twinso '(tofu tofu))
        (== #t q))
      '(#t))

;; Page 32
(test "twinso - 1.revision"
      (run* (z)
        (twinso `(,z tofu)))
      '(tofu))

;; loto stand for list-of-twins
;; Page 33
(define loto
  (lambda (l)
    (conde
     ((nullo l) succeed)
     ((fresh (a)
        (caro l a)
        (twinso a))
      (fresh (d)
        (cdro l d)
        (loto d)))
     (else fail))))

;; Page 33
(test "loto"
      (run 1 (z)
        (loto `((g g) . ,z)))
      '(()))

;; Page 34
(test "loto"
      (run 5 (z)
        (loto `((g g) . ,z)))
      '(()
        ((_.0 _.0))
        ((_.0 _.0) (_.1 _.1))
        ((_.0 _.0) (_.1 _.1) (_.2 _.2))
        ((_.0 _.0) (_.1 _.1) (_.2 _.2) (_.3 _.3))))

;; Page 34
(test "loto"
      (run 5 (r)
        (fresh (w x y z)
          (loto `((g g) (e ,w) (,x ,y) . ,z))
          (== `(,w (,x ,y) ,z) r)))
      '((e (_.0 _.0) ())
        (e (_.0 _.0) ((_.1 _.1)))
        (e (_.0 _.0) ((_.1 _.1) (_.2 _.2)))
        (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3)))
        (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3) (_.4 _.4)))))

;; Page 35
(test "loto"
      (run 3 (out)
        (fresh (w x y z)
          (== `((g g) (e ,w) (,x ,y) . ,z) out)
          (loto out)))
      '(((g g) (e e) (_.0 _.0))
        ((g g) (e e) (_.0 _.0) (_.1 _.1))
        ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

;; Page 35
(define listofo
  (lambda (predo l)
    (conde
     ((nullo l) succeed)
     ((fresh (a)
        (caro l a)
        (predo a))
      (fresh (d)
        (cdro l d)
        (listofo predo d)))
     (else fail))))

;; Page 35
(test "listofo"
      (run 3 (out)
        (fresh (w x y z)
          (== `((g g) (e ,w) (,x ,y) . ,z) out)
          (listofo twinso out)))
      '(((g g) (e e) (_.0 _.0))
        ((g g) (e e) (_.0 _.0) (_.1 _.1))
        ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2))))

;; Page 35
(define loto
  (lambda (l)
    (listofo twinso l)))

;; Page 36
(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

;; Page 36
(define member?
  (lambda (x l)
    (cond
     ((null? l) #f)
     ((eq-car? l x) #t)
     (else (member? x (cdr l))))))

;; Page 36
(define eq-caro
  (lambda (l x)
    (caro l x)))

;; Page 36
(define membero?
  (lambda (x l)
    (conde
     ((nullo l) fail)
     ((eq-caro l x) succeed)
     (else
      (fresh (d)
        (cdro l d)
        (membero? x d))))))

;; Page 36
(test "membero?"
      (run* (q)
        (membero 'olive '(virgin olive oil))
        (== #t q))
      '(#t))

;; Page 36
(test "membero?"
      (run 1 (y)
        (membero y '(hummus with pita)))
      '(hummus))

(test "membero?"
      (run 2 (y)
        (membero y '(hummus with pita)))
      '(hummus with))

;; Page 36
(test "membero?"
      (run 1 (y)
        (membero y '(with pita)))
      '(with))

;; Page 36
(test "membero?"
      (run 1 (y)
        (membero y '(pita)))
      '(pita))

;; Page 36
(test "membero?"
      (run* (y)
        (membero y '()))
      '())

;; Page 36
(test "membero?"
      (run* (y)
        (membero y '(hummus with pita)))
      '(hummus with pita))

;; Page 38
(define identity
  (lambda (l)
    (run* (y)
      (membero y l))))

(test "identity"
      (identity '(a b c))
      '(a b c))

;; Page 38
(test "membero"
      (run* (x)
        (membero 'e `(pasta ,x fagioli)))
      '(e))

;; Reasoning:
;; It returns '_.O' because the recursion succeeds before it gets to
;; the variable x. So (eq-caro l a) in membero? succeeds, which means
;; that no value is associated to the result of this run of membero?
;; and thus the variable stays "fresh" and the result is '_.0'.
;; Page 38
(test "membero"
      (run 1 (x)
        (membero 'e `(pasta e ,x fagioli)))
      '(_.0))

;; Page 39
(test "membero"
      (run* (r)
        (fresh (x y)
          (membero 'e `(pasta ,x fagioli ,y))
          (== `(,x ,y) r)))
      '((e _.0) (_.0 e)))

;; Page 39
(test "membero"
      (run 1 (l)
        (membero 'tofu l))
      '((tofu . _.0)))

(test "membero"
      (run 5 (l)
        (membero 'tofu l))
      '((tofu . _.0)
        (_.0 tofu . _.1)
        (_.0 _.1 tofu . _.2)
        (_.0 _.1 _.2 tofu . _.3)
        (_.0 _.1 _.2 _.3 tofu . _.4)))

;; Page 40
(define pmembero
  (lambda (x l)
    (conde
     ((nullo l) fail)
     ((eq-caro l x) (cdro l '()))
     (else
      (fresh (d)
        (cdro l d)
        (pmembero x d))))))

;; Page 40
(test "pmembero"
      (run 5 (l)
        (pmembero 'tofu l))
      '((tofu)
        (_.0 tofu)
        (_.0 _.1 tofu)
        (_.0 _.1 _.2 tofu)
        (_.0 _.1 _.2 _.3 tofu)))

;; Page 41
(test "pmembero"
      (run* (q)
        (pmembero 'tofu '(a b tofu d tofu))
        (== #t q))
      '(#t))

(test "pmembero"
      (run* (q)
        (pmembero 'tofu '(a b tofu d))
        (== #t q))
      '())

;; 1.revision
;; Page 41
(define pmembero
  (lambda (x l)
    (conde
     ((nullo l) fail)
     ((eq-caro l x) (cdro l '()))
     ((eq-caro l x) succeed)
     (else
      (fresh (d)
        (cdro l d)
        (pmembero x d))))))

;; Page 41
(test "pmembero - 1.revision"
      (run* (q)
        (pmembero 'tofu '(a b tofu d tofu))
        (== #t q))
      '(#t #t #t))

;; 2.revision
;; Page 42
(define pmembero
  (lambda (x l)
    (conde
     ((nullo l) fail)
     ((eq-caro l x) (cdro l '()))
     ((eq-caro l x)
      (fresh (a d)
        ;; Makes sure that the cdr is not the empty list.
        (cdro l `(,a . ,d))))
     (else
      (fresh (d)
        (cdro l d)
        (pmembero x d))))))

;; Page 42
(test "pmembero - 2.revision"
      (run* (q)
        (pmembero 'tofu '(a b tofu d tofu))
        (== #t q))
      '(#t #t))

;; Page 42
(test "pmembero - 2.revision"
      (run 12 (l)
        (pmembero 'tofu l))
      '((tofu)
        (tofu _.0 . _.1)
        (_.0 tofu)
        (_.0 tofu _.1 . _.2)
        (_.0 _.1 tofu)
        (_.0 _.1 tofu _.2 . _.3)
        (_.0 _.1 _.2 tofu)
        (_.0 _.1 _.2 tofu _.3 . _.4)
        (_.0 _.1 _.2 _.3 tofu)
        (_.0 _.1 _.2 _.3 tofu _.4 . _.5)
        (_.0 _.1 _.2 _.3 _.4 tofu)
        (_.0 _.1 _.2 _.3 _.4 tofu _.5 . _.6)))

;; 3.revision
;; Page 43
(define pmembero
  (lambda (x l)
    (conde
     ((eq-caro l x)
      (fresh (a d)
        ;; Makes sure that the cdr is not the empty list.
        (cdro l `(,a . ,d))))
     ((eq-caro l x) (cdro l '()))
     (else
      (fresh (d)
        (cdro l d)
        (pmembero x d))))))

;; Page 42
(test "pmembero - 3.revision"
      (run* (q)
        (pmembero 'tofu '(a b tofu d tofu))
        (== #t q))
      '(#t #t))

;; Page 42
(test "pmembero - 3.revision"
      (run 12 (l)
        (pmembero 'tofu l))
      '((tofu _.0 . _.1)
        (tofu)
        (_.0 tofu _.1 . _.2)
        (_.0 tofu)
        (_.0 _.1 tofu _.2 . _.3)
        (_.0 _.1 tofu)
        (_.0 _.1 _.2 tofu _.3 . _.4)
        (_.0 _.1 _.2 tofu)
        (_.0 _.1 _.2 _.3 tofu _.4 . _.5)
        (_.0 _.1 _.2 _.3 tofu)
        (_.0 _.1 _.2 _.3 _.4 tofu _.5 . _.6)
        (_.0 _.1 _.2 _.3 _.4 tofu)))

;; Page 44
(define first-value
  (lambda (l)
    (run 1 (y)
      (membero y l))))

;; Page 44
(test "first-value"
      (first-value '(pasta e fagioli))
      '(pasta))

(test "first-value"
      (first-value '())
      '())

;; Page 44
(define memberrevo
  (lambda (x l)
    (conde
     ((nullo l) fail)
     (succeed (fresh (d)
                (cdro l d)
                (memberrevo x d)))
     (else (eq-caro l x)))))

;; Page 44
(test "memberrevo"
      (run* (x)
        (memberrevo x '(pasta e fagioli)))
      '(fagioli e pasta))

;; Page 45
(define reverse-list
  (lambda (l)
    (run* (y)
      (memberrevo y l))))

(test "reverse-list"
      (reverse-list '(a b c))
      '(c b a))

(test "reverse-list"
      (reverse-list '())
      '())

;; 1.revision
(define memberrevo
  (lambda (x l)
    (conde
     (succeed (fresh (d)
                (cdro l d)
                (memberrevo x d)))
     (else (eq-caro l x)))))

(test "memberrevo - 1.revision"
      (run* (x)
        (memberrevo x '(pasta e fagioli)))
      '(fagioli e pasta))

;; 2.revision
(define memberrevo
  (lambda (x l)
    (conde
     ((fresh (d)
        (cdro l d)
        (memberrevo x d))
      succeed)
     (else (eq-caro l x)))))

(test "memberrevo - 2.revision"
      (run* (x)
        (memberrevo x '(pasta e fagioli)))
      '(fagioli e pasta))
