;; Chapter 4: Members only

(load "test-check.scm")
(load "minikanren/mkprelude.scm")

;; Page 36, Chaper 3
(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

;; Page 47
(define mem
  (lambda (x l)
    (cond
     ((null? l) #f)
     ((eq-car? l x) l)
     (else (mem x (cdr l))))))

;; Page 47
(test "mem"
      (mem 'tofu '(a b tofu d peas e))
      '(tofu d peas e))

;; Page 47
(test "mem"
      (mem 'tofu '(a b peas d peas e))
      #f)

;; Page 47
(test "mem"
      (run* (out)
        (== (mem 'tofu '(a b tofu d peas e)) out))
      '((tofu d peas e)))

;; Page 47
(test "mem"
      (mem 'peas (mem 'tofu '(a b tofu d peas e)))
      '(peas e))

;; Page 47
(test "mem"
      (mem 'tofu (mem 'tofu '(a b tofu d tofu e)))
      '(tofu d tofu e))

;; Page 47
(test "mem"
      (mem 'tofu (cdr (mem 'tofu '(a b tofu d tofu e))))
      '(tofu e))

;; Page 48
(define memo
  (lambda (x l out)
    (conde
     ((nullo l) fail)
     ((eq-caro l x) (== l out))
     (else
      (fresh (d)
        (cdro l d)
        (memo x d out))))))

;; Page 48
(test "memo"
      (run 1 (out)
        (memo 'tofu '(a b tofu d tofu e) out))
      '((tofu d tofu e)))

;; Page 48
(test "memo"
      (run 1 (out)
        (fresh (x)
          (memo 'tofu `(a b ,x d tofu e) out)))
      '((tofu d tofu e)))

;; Page 49
(test "memo"
      (run* (r)
        (memo r '(a b tofu d tofu e) '(tofu d tofu e)))
      '(tofu))

;; Page 49
(test "memo"
      (run* (q)
        (memo 'tofu '(tofu e) '(tofu e))
        (== #t q))
      '(#t))

;; Page 49
(test "memo"
      (run* (q)
        (memo 'tofu '(tofu e) '(tofu))
        (== #t q))
      '())

;; Page 49
(test "memo"
      (run* (x)
        (memo 'tofu '(tofu e) `(,x e)))
      '(tofu))

;; Page 49
(test "memo"
      (run* (out)
        (fresh (x)
          (memo 'tofu `(a b ,x d tofu e) out)))
      '((tofu d tofu e) (tofu e)))

;; Page 50
(test "memo"
      (run 12 (z)
        (fresh (u)
          (memo 'tofu `(a b tofu d tofu . ,z) u)))
      '(_.0
        _.0
        (tofu . _.0)
        (_.0 tofu . _.1)
        (_.0 _.1 tofu . _.2)
        (_.0 _.1 _.2 tofu . _.3)
        (_.0 _.1 _.2 _.3 tofu . _.4)
        (_.0 _.1 _.2 _.3 _.4 tofu . _.5)
        (_.0 _.1 _.2 _.3 _.4 _.5 tofu . _.6)
        (_.0 _.1 _.2 _.3 _.4 _.5 _.6 tofu . _.7)
        (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 tofu . _.8)
        (_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8 tofu . _.9)))

;; 1.revision
;; Page 50
(define memo
  (lambda (x l out)
    (conde
     ((eq-caro l x) (== l out))
     (else
      (fresh (d)
        (cdro l d)
        (memo x d out))))))

(test "memo - 1.revision"
      (run* (q)
        (memo 'tofu '(tofu e) '(tofu e))
        (== #t q))
      '(#t))

(test "memo - 1.revision"
      (run* (q)
        (memo 'tofu '(tofu e) '(tofu))
        (== #t q))
      '())

(test "memo - 1.revision"
      (run* (x)
        (memo 'tofu '(tofu e) `(,x e)))
      '(tofu))

(test "memo - 1.revision"
      (run* (out)
        (fresh (x)
          (memo 'tofu `(a b ,x d tofu e) out)))
      '((tofu d tofu e) (tofu e)))

;; Page 51
(define rember
  (lambda (x l)
    (cond
     ((null? l) '())
     ((eq-car? l x) (cdr l))
     (else
      (cons (car l) (rember x (cdr l)))))))

(test "rember"
      (rember 'x '())
      '())

;; Page 51
(test "rember"
      (rember 'peas '(a b peas d peas e))
      '(a b d peas e))

;; Page 51
(define rembero
  (lambda (x l out)
    (conde
     ((nullo l) (== '() out))
     ((eq-caro l x) (cdro l out))
     (else
      (fresh (res)
        (fresh (d)
          (cdro l d)
          (rembero x d res))
        (fresh (a)
          (caro l a)
          (conso a res out)))))))

(test "rembero"
      (run 1 (out)
        (fresh (y)
          (rembero 'peas `(a b ,y d peas e) out)))
      '((a b d peas e)))

;; rembero: [s-exp] list list -> [goal]
;; 1.revision
(define rembero
  (lambda (x l out)
    (conde
     ((nullo l) (== '() out))
     ((eq-caro l x) (cdro l out))
     (else
      (fresh (a d res)
        (cdro l d)
        (rembero x d res)
        (caro l a)
        (conso a res out))))))

(test "rembero - 1.revision"
      (run 1 (out)
        (fresh (y)
          (rembero 'peas `(a b ,y d peas e) out)))
      '((a b d peas e)))

;; 2.revision
(define rembero
  (lambda (x l out)
    (conde
     ((nullo l) (== '() out))
     ((eq-caro l x) (cdro l out))
     (else
      (fresh (a d res)
        (conso a d l)
        (rembero x d res)
        (conso a res out))))))

;; Page 52
(test "rembero - 2.revision"
      (run 1 (out)
        (fresh (y)
          (rembero 'peas `(a b ,y d peas e) out)))
      '((a b d peas e)))

;; Page 53
(test "rembero - 2.revision"
      (run* (out)
        (fresh (y z)
          (rembero y `(a b ,y d ,z e) out)))
      '((b a d _.0 e)
        (a b d _.0 e)
        (a b d _.0 e)
        (a b d _.0 e)
        (a b _.0 d e)
        (a b e d _.0)
        (a b _.0 d _.1 e)))

;; Page 55
(test "rembero - 2.revision"
      (run* (r)
        (fresh (y z)
          (rembero y `(,y d ,z e) `(,y d e))
          (== `(,y ,z) r)))
      '((d d) (d d) (_.0 _.0) (e e)))

;; Page 56
(test "rembero - 2.revision"
      (run 13 (w)
        (fresh (y z out)
          (rembero y `(a b ,y d ,z . ,w) out)))
      '(_.0
        _.0
        _.0
        _.0
        _.0
        ()
        (_.0 . _.1)
        (_.0)
        (_.0 _.1 . _.2)
        (_.0 _.1)
        (_.0 _.1 _.2 . _.3)
        (_.0 _.1 _.2)
        (_.0 _.1 _.2 _.3 . _.4)))

;; Page 58
(define surpriso
  (lambda (s)
    (rembero s '(a b c) '(a b c))))

;; Page 58
(test "surpriso"
      (run* (r)
        (== 'd r)
        (surpriso r))
      '(d))

;; Page 58
(test "surpriso"
      (run* (r)
        (surpriso r))
      '(_.0))

;; Page 59
(test "surpriso"
      (run* (r)
        (surpriso r)
        (== 'b r))
      '(b))
