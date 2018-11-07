;; Chapter 5: Double Your Fun

(load "test-check.scm")
(load "minikanren/mkprelude.scm")

;; Page 61
(define append
  (lambda (l s)
    (cond
     ((null? l) s)
     (else (cons (car l)
             (append (cdr l) s))))))

;; Page 61
(test "append"
      (append '(a b c) '(d e))
      '(a b c d e))

;; Page 61
(test "append"
      (append '(a b c) '())
      '(a b c))

;; Page 61
(test "append"
      (append '() '(d e))
      '(d e))

;; Page 61
(test "append"
      (append '(d e) 'a)
      '(d e . a))

;; Page 62
(define appendo
  (lambda (l s out)
    (conde
     ((nullo l) (== s out))
     (else
      (fresh (a d res)
        (caro l a)
        (cdro l d)
        (appendo d s res)
        (conso a res out))))))

;; Page 62
(test "appendo"
      (run* (x)
        (appendo '(cake with ice) '(tastes yummy) x))
      '((cake with ice tastes yummy)))

;; Page 62
(test "appendo"
      (run* (x)
        (fresh (y)
          (appendo `(cake with ice ,y) '(tastes yummy) x)))
      '((cake with ice _.0 tastes yummy)))

;; Page 62
(test "appendo"
      (run* (x)
        (fresh (y)
          (appendo '(cake with ice cream) y x)))
      '((cake with ice cream . _.0)))

;; Page 62
(test "appendo"
      (run 1 (x)
        (fresh (y)
          (appendo `(cake with ice . ,y) '(d t) x)))
      '((cake with ice d t)))

;; Page 62
(test "appendo"
      (run 1 (y)
        (fresh (x)
          (appendo `(cake with ice . ,y) '(d t) x)))
      '(()))

;; 1.revision
;; Page 62
(define appendo
  (lambda (l s out)
    (conde
     ((nullo l) (== s out))
     (else
      (fresh (a d res)
        (conso a d l)
        (appendo d s res)
        (conso a res out))))))

;; Page 63
(test "appendo - 1.revision"
      (run 5 (x)
        (fresh (y)
          (appendo `(cake with ice . ,y) '(d t) x)))
      '((cake with ice d t)
        (cake with ice _.0 d t)
        (cake with ice _.0 _.1 d t)
        (cake with ice _.0 _.1 _.2 d t)
        (cake with ice _.0 _.1 _.2 _.3 d t)))

;; Page 63
(test "appendo - 1.revision"
      (run 5 (y)
        (fresh (x)
          (appendo `(cake with ice . ,y) '(d t) x)))
      '(()
        (_.0)
        (_.0 _.1)
        (_.0 _.1 _.2)
        (_.0 _.1 _.2 _.3)))

;; Page 64
(test "appendo - 1.revision"
      (run 5 (x)
        (fresh (y)
          (appendo `(cake with ice . ,y) `(d t . ,y) x)))
      '((cake with ice d t)
        (cake with ice _.0 d t _.0)
        (cake with ice _.0 _.1 d t _.0 _.1)
        (cake with ice _.0 _.1 _.2 d t _.0 _.1 _.2)
        (cake with ice _.0 _.1 _.2 _.3 d t _.0 _.1 _.2 _.3)))

;; Page 64
(test "appendo - 1.revision"
      (run* (x)
        (fresh (z)
          (appendo '(cake with ice cream) `(d t . ,z) x)))
      '((cake with ice cream d t . _.0)))

;; Page 64
(test "appendo - 1.revision"
      (run 6 (x)
        (fresh (y)
          (appendo x y '(cake with ice d t))))
      '(()
        (cake)
        (cake with)
        (cake with ice)
        (cake with ice d)
        (cake with ice d t)))

;; Page 65
(test "appendo - 1.revision"
      (run 6 (y)
        (fresh (x)
          (appendo x y '(cake with ice d t))))
      '((cake with ice d t)
        (with ice d t)
        (ice d t)
        (d t)
        (t)
        ()))

;; Page 65
(test "appendo - 1.revision"
      (run 6 (r)
        (fresh (x y)
          (appendo x y '(cake with ice d t))
          (== `(,x ,y) r)))
      '((() (cake with ice d t))
        ((cake) (with ice d t))
        ((cake with) (ice d t))
        ((cake with ice) (d t))
        ((cake with ice d) (t))
        ((cake with ice d t) ())))

;; 2.revision
;; Page 66
(define appendo
  (lambda (l s out)
    (conde
     ((nullo l) (== s out))
     (else
      (fresh (a d res)
        (conso a d l)
        (conso d res out)
        (appendo d s res))))))

;; If you use this test with the 1.revision of appendo the computation
;; would not stop.
;; Page 66
(test "appendo - 2.revision"
      (run 7 (r)
        (fresh (x y)
          (appendo x y '(cake with ice d t))
          (== `(,x ,y) r)))
      '((() (cake with ice d t))))

;; 1.revision
;; Page 62
(define appendo
  (lambda (l s out)
    (conde
     ((nullo l) (== s out))
     (else
      (fresh (a d res)
        (conso a d l)
        (appendo d s res)
        (conso a res out))))))

;; Page 66
(test "appendo - 1.revision"
      (run 7 (x)
        (fresh (y z)
          (appendo x y z)))
      '(()
        (_.0)
        (_.0 _.1)
        (_.0 _.1 _.2)
        (_.0 _.1 _.2 _.3)
        (_.0 _.1 _.2 _.3 _.4)
        (_.0 _.1 _.2 _.3 _.4 _.5)))

;; Page 66
(test "appendo - 1.revision"
      (run 7 (y)
        (fresh (x z)
          (appendo x y z)))
      '(_.0 _.0 _.0 _.0 _.0 _.0 _.0))

;; Page 67
(test "appendo - 1.revision"
      (run 7 (z)
        (fresh (x y)
          (appendo x y z)))
      '(_.0
        (_.0 . _.1)
        (_.0 _.1 . _.2)
        (_.0 _.1 _.2 . _.3)
        (_.0 _.1 _.2 _.3 . _.4)
        (_.0 _.1 _.2 _.3 _.4 . _.5)
        (_.0 _.1 _.2 _.3 _.4 _.5 . _.6)))

;; Page 67
(test "appendo - 1.revision"
      (run 7 (r)
        (fresh (x y z)
          (appendo x y z)
          (== `(,x ,y ,z) r)))
      '((() _.0 _.0)
        ((_.0) _.1 (_.0 . _.1))
        ((_.0 _.1) _.2 (_.0 _.1 . _.2))
        ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
        ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
        ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))
        ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))))

;; Page 67
(define swappendo
  (lambda (l s out)
    (conde
     (succeed (fresh (a d res)
                (conso a d l)
                (conso a res out)
                (swappendo d s res)))
     (else (nullo l) (== (s out))))))

;; Page 68
(define unwrap
  (lambda (x)
    (cond
     ((pair? x) (unwrap (car x)))
     (else x))))

;; Page 68
(test "unwrap"
      (unwrap '((((pizza)))))
      'pizza)

;; Page 68
(test "unwrap"
      (unwrap '((((pizza pie) with)) extra cheese))
      'pizza)

;; Page 69
(define unwrapo
  (lambda (x out)
    (conde
     ((pairo x) (fresh (a)
                  (caro x a)
                  (unwrapo a out)))
     (else (== x out)))))

;; Page 69
(test "unwrapo"
      (run*  (x)
        (unwrapo '(((pizza))) x))
      '(pizza
        (pizza)
        ((pizza))
        (((pizza)))))

;; 1.revision
;; Page 70
(define unwrapo
  (lambda (x out)
    (conde
     (succeed (== x out))
     (else
      (fresh (a)
        (caro x a)
        (unwrapo a out))))))

;; Page 70
(test "unwrapo - 1.revision"
      (run 5 (x)
        (unwrapo x 'pizza))
      '(pizza
        (pizza . _.0)
        ((pizza . _.0) . _.1)
        (((pizza . _.0) . _.1) . _.2)
        ((((pizza . _.0) . _.1) . _.2) . _.3)))

;; Page 70
(test "unwrapo - 1.revision"
      (run 5 (x)
        (unwrapo x '((pizza))))
      '(((pizza))
        (((pizza)) . _.0)
        ((((pizza)) . _.0) . _.1)
        (((((pizza)) . _.0) . _.1) . _.2)
        ((((((pizza)) . _.0) . _.1) . _.2) . _.3)))

;; Page 70
(test "unwrapo - 1.revision"
      (run 5 (x)
        (unwrapo `((,x)) 'pizza))
      '(pizza
        (pizza . _.0)
        ((pizza . _.0) . _.1)
        (((pizza . _.0) . _.1) . _.2)
        ((((pizza . _.0) . _.1) . _.2) . _.3)))

;; Page 71
(define flatten
  (lambda (s)
    (cond
     ((null? s) '())
     ((pair? s) (append
                 (flatten (car s))
                 (flatten (cdr s))))
     (else (cons s '())))))

;; Page 71
(test "flatten"
      (flatten '((a b) c))
      '(a b c))

;; Page 71
(define flatteno
  (lambda (s out)
    (conde
     ((nullo s) (== '() out))
     ((pairo s) (fresh (a d res-a res-d)
                  (caro s a)
                  (flatteno a res-a)
                  (cdro s d)
                  (flatteno d res-d)
                  (appendo res-a res-d out)))
     (else (conso s '() out)))))

;; Page 71
(test "flatteno"
      (run 1 (x)
        (flatteno '((a b) c) x))
      '((a b c)))

;; Page 71
(test "flatteno"
      (run 1 (x)
        (flatteno '(a (b c)) x))
      '((a b c)))

(test "flatteno"
      (run 2 (x)
        (flatteno '(a (b c)) x))
      '((a b c)
        (a b c ())))

(test "flatteno"
      (run 3 (x)
        (flatteno '(a (b c)) x))
      '((a b c)
        (a b c ())
        (a b c ())))

;; Reasoning: So where does these values come from?
;;
;; The value '((a)) comes from the 3. conde line (... (else (conso s
;; '() out))) of the expression (flatteno '(a) x). After (pairo s)
;; succeeds we pretend that it failed and evaluate the 'else' cond
;; line which returns the value '((a)).
;;
;; The values '(a) and '(a ()) come from the 2. conde line ((pairo s)
;; ...). The first recursive call to flatteno (flatteno 'a res-a)
;; returns the value '(a). The next recursive call to flatteno
;; (flatteno '() res-b) returns two values; '() and '(()). So (appendo
;; res-a res-d out) becomes (appendo '(a) '() out) which returns '(a)
;; and (appendo '(a) '(()) out) which returns '(a ()).
;;
;; Page 72
(test "flatteno"
      (run* (x)
        (flatteno '(a) x))
      '((a) (a ()) ((a))))

;; Page 72
(test "flatteno"
      (run* (x)
        (flatteno '((a)) x))
      '((a)
        (a ())
        (a ())
        (a () ())
        ((a))
        ((a) ())
        (((a)))))

;; Page 72
(test "flatteno"
      (run* (x)
        (flatteno '(((a))) x))
      '((a)
        (a ())
        (a ())
        (a () ())
        (a ())
        (a () ())
        (a () ())
        (a () () ())
        ((a))
        ((a) ())
        ((a) ())
        ((a) () ())
        (((a)))
        (((a)) ())
        ((((a))))))

;; Page 73
(test "flatteno"
      (run* (x)
        (flatteno '((a b) c) x))
      '((a b c)
        (a b c ())
        (a b (c))
        (a b () c)
        (a b () c ())
        (a b () (c))
        (a (b) c)
        (a (b) c ())
        (a (b) (c))
        ((a b) c)
        ((a b) c ())
        ((a b) (c))
        (((a b) c))))

;; Page 74
(define flattenrevo
  (lambda (s out)
    (conde
     (succeed (conso s '() out))
     ((nullo s) (== '() out))
     (else
      (fresh (a d res-a res-d)
        (conso a d s)
        (flattenrevo a res-a)
        (flattenrevo d res-d)
        (appendo res-a res-d out))))))

;; Page 74
(test "flattenrevo"
      (run* (x)
        (flattenrevo '((a b) c) x))
      '((((a b) c))
        ((a b) (c))
        ((a b) c ())
        ((a b) c)
        (a (b) (c))
        (a (b) c ())
        (a (b) c)
        (a b () (c))
        (a b () c ())
        (a b () c)
        (a b (c))
        (a b c ())
        (a b c)))

;; Page 74
(test "flattenrevo"
      (reverse
       (run* (x)
         (flattenrevo '((a b) c) x)))
      '((a b c)
        (a b c ())
        (a b (c))
        (a b () c)
        (a b () c ())
        (a b () (c))
        (a (b) c)
        (a (b) c ())
        (a (b) (c))
        ((a b) c)
        ((a b) c ())
        ((a b) (c))
        (((a b) c))))

;; Page 75
(test "flattenrevo"
      (run 2 (x)
        (flattenrevo x '(a b c)))
      '((a b . c) (a b c)))

;; Page 75
(test "flattenrevo"
      (length
       (run* (x)
         (flattenrevo '((((a (((b))) c))) d) x)))
      574)
