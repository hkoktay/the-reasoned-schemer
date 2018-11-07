;; Chapter 2: Teaching Old Toys New Tricks

(load "test-check.scm")
(load "minikanren/mkprelude.scm")

;; Page 17
(test "let"
      (let ((x (lambda (a) a))
            (y 'c))
        (x y))
      'c)

;; Page 17
(test "lists as variables"
      (run* (r)
        (fresh (y x)
          (== `(,x ,y) r)))
      '((_.0 _.1)))

;; Page 17
(test "lists as variables"
      (run* (r)
        (fresh (v w)
          (== (let ((x v)
                    (y w))
                `(,x ,y))
              r)))
      '((_.0 _.1)))

;; Page 18
(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

;; Page 17
(test "caro"
      (run* (r)
        (caro '(a c o r n) r))
      '(a))

;; Page 18
(test "caro"
      (run* (q)
        (caro '(a c o r n) 'a)
        (== #t q))
      '(#t))

;; Page 18
(test "caro"
      (run* (r)
        (fresh (x y)
          (caro '(grape raisin pear) x)
          (caro '((a) (b) (c)) y)
          (== (cons x y) r)))
      '((grape a)))

;; Page 19
(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

;; Page 19
(test "cdro"
      (run* (r)
        (fresh (v)
          (cdro '(a c o r n) v)
          (caro v r)))
      '(c))

;; Page 19
(test "cdro"
      (run* (r)
        (fresh (x y)
          (cdro '(grape raisin pear) x)
          (caro '((a) (b) (c)) y)
          (==  (cons x y) r)))
      '(((raisin pear) a)))

;; Page 20
(test "cdro"
      (run* (q)
        (cdro '(a c o r n) '(c o r n))
        (== #t q))
      '(#t))

;; Page 20
(test "cdro"
      (run* (x)
        (cdro '(c o r n) `(,x r n)))
      '(o))

;; Page 20
(test "cdro"
      (run* (l)
        (fresh (x)
          (cdro l '(c o r n))
          (caro l x)
          (== 'a x)))
      '((a c o r n)))

;; Page 20
(test "conso"
      (run* (l)
        (conso '(a b c) '(d e) l))
      '(((a b c) d e)))

;; Page 20
(test "conso"
      (run* (x)
        (conso x '(a b c) '(d a b c)))
      '(d))

;; Page 20
(test "conso"
      (run* (r)
        (fresh (x y z)
          (== `(e a d ,x) r)
          (conso y `(a ,z c) r)))
      '((e a d c)))

;; Reasoning:
;; `(,x a ,x c)
;; `( d a ,x c)
;; -> '(d a d c)
;; -> d
;; Page 20
(test "conso"
      (run* (x)
        (conso x `(a ,x c) `(d a ,x c)))
      '(d))

;; This time we are looking for the complete list. 
;; Page 21
(test "conso"
      (run* (l)
        (fresh (x)
          (== `(d a ,x c) l)
          (conso x `(a ,x c) l)))
      '((d a d c)))

;; Page 21
(test "conso"
      (run* (l)
        (fresh (x)
          (conso x `(a ,x c) l)
          (== `(d a ,x c) l)))
      '((d a d c)))

;; Page 21
(define conso
  (lambda (a d p)
    (== (cons a d) p)))

;; Reasoning:
;; (conso w '(a n s) s) -> `(,w a n s)
;; (cdro l s)  -> `(,w a n s)
;;             -> l must be a list with 5 elements
;; (caro l x)  -> `(,x ,w a n s)
;; (== 'b x)   -> `(b ,w a n s)            
;; (cdro l d)  -> `(,w a n s)
;; (caro d y)  -> ,w
;; (== 'e y)   -> ,w is 'e
;; -> l is '(b e a n s)
;; Page 21
(test "conso, caro and cdro"
      (run* (l)
        (fresh (d x y w s)
          (conso w '(a n s) s)
          (cdro l s)
          (caro l x)
          (== 'b x)
          (cdro l d)
          (caro d y)
          (== 'e y)))
      '((b e a n s)))

;; Page 22
(test "nullo"
      (run* (q)
        (nullo '(grape raisin pear))
        (== #t q))
      '())

;; Page 22
(test "nullo"
      (run* (q)
        (nullo '())
        (== #t q))
      '(#t))

;; Page 22
(test "nullo"
      (run* (x)
        (nullo x))
      '(()))

;; Page 22
(define nullo
  (lambda (x)
    (== x '())))

;; Page 22
(test "eqo"
      (run* (q)
        (eqo 'pear 'plum)
        (== #t q))
      '())

;; Page 23
(test "eqo"
      (run* (q)
        (eqo 'plum 'plum)
        (== #t q))
      '(#t))

;; Page 23
(define eqo
  (lambda (x y)
    (== x y)))

;; Page 24
(test "make pair"
      (run* (r)
        (fresh (x y)
          (== (cons x (cons y 'salad)) r)))
      '((_.0 _.1 . salad)))

;; Page 24
(define pairo
  (lambda (p)
    (fresh (a d)
      (conso a d p))))

;; Page 24
(test "pairo"
      (run* (q)
        (pairo (cons q q))
        (== #t q))
      '(#t))

;; Page 24
(test "pairo"
      (run* (q)
        (pairo '())
        (== #t q))
      '())

;; Page 25
(test "pairo"
      (run* (q)
        (pairo 'pair)
        (== #t q))
      '())

;; Page 35
(test "pairo"
      (run* (x)
        (pairo x))
      '((_.0 . _.1)))

;; Page 35
(test "pairo"
      (run* (r)
        (pairo (cons r 'pear)))
      '(_.0))
