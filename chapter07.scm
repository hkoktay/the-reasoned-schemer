;; Chapter 7: A Bit Too Much

(load "test-check.scm")
(load "minikanren/mkprelude.scm")

;; Page 87
(define bit-xoro
  (lambda (x y r)
    (conde
     ((== 0 x) (== 0 y) (== 0 r))
     ((== 1 x) (== 0 y) (== 1 r))
     ((== 0 x) (== 1 y) (== 1 r))
     ((== 1 x) (== 1 y) (== 0 r))
     (else fail))))

;; Another version of bit-xoro with bit-nando
;; (define bit-xoro
;;   (lambda (x y r)
;;     (fresh (s t u)
;;       (bit-nando x y s)
;;       (bit-nando x s t)
;;       (bit-nando s y u)
;;       (bit-nando t u r))))

;; (define bit-nando
;;   (lambda (x y r)
;;     (conde
;;      ((== 0 x) (== 0 y) (== 1 r))
;;      ((== 1 x) (== 0 y) (== 1 r))
;;      ((== 0 x) (== 1 y) (== 1 r))
;;      ((== 1 x) (== 1 y) (== 0 r))
;;      (else fail))))


;; Page 87
(test "bit-xoro"
      (run* (s)
        (fresh (x y)
          (bit-xoro x y 0)
          (== `(,x ,y) s)))
      '((0 0) (1 1)))

;; Page 88
(test "bit-xoro"
      (run* (s)
        (fresh (x y)
          (bit-xoro x y 1)
          (== `(,x ,y) s)))
      '((1 0) (0 1)))

;; Page 88
(test "bit-xoro"
      (run* (s)
        (fresh (x y r)
          (bit-xoro x y r)
          (== `(,x ,y ,r) s)))
      '((0 0 0)
        (1 0 1)
        (0 1 1)
        (1 1 0)))

;; Page 88
(define bit-ando
  (lambda (x y r)
    (conde
     ((== 0 x) (== 0 y) (== 0 r))
     ((== 1 x) (== 0 y) (== 0 r))
     ((== 0 x) (== 1 y) (== 0 r))
     ((== 1 x) (== 1 y) (== 1 r))
     (else fail))))

;; Another way to define bit-ando is to use bit-nando and bit-noto
;; (define bit-ando
;;   (lambda (x y r)
;;     (fresh (s)
;;       (bit-nando x y s)
;;       (bit-noto s r))))

;; (define bit-noto
;;   (lambda (x r)
;;     (bit-nando x x r)))

;; Page 88
(test "bit-ando"
      (run* (s)
        (fresh (x y)
          (bit-ando x y 1)
          (== `(,x ,y) s)))
      '((1 1)))

;; Page 89
(define half-addero
  (lambda (x y r c)
    (all
     (bit-xoro x y r)
     (bit-ando x y c))))

;; Alternative definition of half-addero
;; (define half-addero
;;   (lambda (x y r c) 
;;   (conde 
;;    ((== 0 x ) (== 0 y) (== 0 r) (== 0 c)) 
;;    ((== 1 x ) (== 0 y) (== 1 r) (== 0 c)) 
;;    ((== 0 x ) (== 1 y) (== 1 r) (== 0 c)) 
;;    ((== 1 x ) (== 1 y) (== 0 r) (== 1 c)) 
;;    (else #u)))) 

;; Page 89
(test "half-addero"
      (run* (r)
        (half-addero 1 1 r 1))
      '(0))

;; Page 89
(test "half-addero"
      (run* (s)
        (fresh (x y r c)
          (half-addero x y r c)
          (== `(,x ,y ,r ,c) s)))
      '((0 0 0 0)
        (1 0 1 0)
        (0 1 1 0)
        (1 1 0 1)))

;; Page 89
(define full-addero
  (lambda (b x y r c)
    (fresh (w xy wz)
      (half-addero x y w xy)
      (half-addero w b r wz)
      (bit-xoro xy wz c))))

;; Alternative definition of full-addero
;; (define full-addero
;;   (lambda (b x y r c) 
;;     (conde 
;;      ((== 0 b) (== 0 x ) (== 0 y) (== 0 r) (== 0 c)) 
;;      ((== 1 b) (== 0 x ) (== 0 y) (== 1 r) (== 0 c)) 
;;      ((== 0 b) (== 1 x ) (== 0 y) (== 1 r) (== 0 c)) 
;;      ((== 1 b) (== 1 x ) (== 0 y) (== 0 r) (== 1 c)) 
;;      ((== 0 b) (== 0 x ) (== 1 y) (== 1 r) (== 0 c)) 
;;      ((== 1 b) (== 0 x ) (== 1 y) (== 0 r) (== 1 c)) 
;;      ((== 0 b) (== 1 x ) (== 1 y) (== 0 r) (== 1 c)) 
;;      ((== 1 b) (== 1 x ) (== 1 y) (== 1 r) (== 1 c)) 
;;      (else fail)))) 

;; Page 89
(test "full-addero"
      (run* (s)
        (fresh (r c)
          (full-addero 0 1 1 r c)
          (== `(,r ,c) s)))
      '((0 1)))

;; Page 90
(test "full-addero"
      (run* (s)
        (fresh (r c)
          (full-addero 1 1 1 r c)
          (== `(,r ,c) s)))
      '((1 1)))

;; Page 90
(test "full-addero"
      (run* (s)
        (fresh (b x y r c)
          (full-addero b x y r c)
          (== `(,b ,x ,y ,r ,c) s)))
      '((0 0 0 0 0)
        (1 0 0 1 0)
        (0 1 0 1 0)
        (1 1 0 0 1)
        (0 0 1 1 0)
        (1 0 1 0 1)
        (0 1 1 0 1)
        (1 1 1 1 1)))

;; add1: [number] -> [number]
(define add1
  (lambda (n)
    (+ n 1)))

(test "add1"
      (add1 67) 68)

(test "add1"
      (add1 0) 1)

(test "add1"
      (add1 -4) -3)

;; sub1: [number] -> [number]
(define sub1
  (lambda (n)
    (- n 1)))

(test "sub1"
      (sub1 10) 9)

(test "sub1"
      (sub1 -4) -5)

;; o<: [number] [number] -> boolean
;; Returns #t if n is smaller than m, else #f-
;;
;; Page 73, The Little Schemer
(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o< (sub1 n) (sub1 m))))))

(test "o<"
      (o< 2 4)
      #t)

(test "o<"
      (o< 4 2)
      #f)

(test "o<"
      (o< 2 2)
      #f)

;; o-: [number] [number] -> [number]
;; Page 61, The Little Schemer
(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(test "o-"
      (o- 5 2)  3)

(test "o-"
      (o- 2 5)  -3)

;; oquotient: [number] [number] -> [number]
;; Division of 'n' and 'm'.
;; 
;; Example: (oquotient 15 4) -> 3
;; Page 75, The Little Schemer
(define oquotient
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else (add1 (oquotient (o- n m) m))))))

;; build-num: [number] -> list
;; Page 93
(define build-num
  (lambda (n)
    (cond
     ((zero? n) '())
     ((and (not (zero? n)) (even? n))
      (cons 0
        (build-num (oquotient n 2))))
     ((odd? n)
      (cons 1
        (build-num (oquotient (sub1 n) 2)))))))

;; Page 92
(test "build-num"
      (build-num 0)
      '())

;; Page 92
(test "build-num"
      (build-num 36)
      '(0 0 1 0 0 1))

(test "build-num"
      (build-num 19)
      '(1 1 0 0 1))

;; build-num: [number] -> list
;; 1.revision
;; Page 93
(define build-num
  (lambda (n)
    (cond
     ((odd? n)
      (cons 1
        (build-num (oquotient (sub1 n) 2))))
     ((and (not (zero? n)) (even? n))
      (cons 0
        (build-num (oquotient n 2))))
     ((zero? n) '()))))

;; Page 92
(test "build-num - 1.revision"
      (build-num 0)
      '())

;; Page 92
(test "build-num - 1.revision"
      (build-num 36)
      '(0 0 1 0 0 1))

(test "build-num - 1.revision"
      (build-num 19)
      '(1 1 0 0 1))

;; Page 97
(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

;; Page 97
(test "poso"
      (run* (q)
        (poso '(0 0 1))
        (== #t q))
      '(#t))

;; Page 97
(test "poso"
      (run* (q)
        (poso '(1))
        (== #t q))
      '(#t))

;; Page 98
(test "poso"
      (run* (q)
        (poso '())
        (== #t q))
      '())

;; Page 98
(test "poso"
      (run* (r)
        (poso r))
      '((_.0 . _.1)))

;; Page 98
(define >1o
  (lambda (n)
    (fresh (a ad dd)
      (== `(,a ,ad . ,dd) n))))

;; Page 98
(test ">1o"
      (run* (q)
        (>1o '(0 1 1))
        (== #t q))
      '(#t))

;; Page 98
(test ">1o"
      (run* (q)
        (>1o '(0 1))
        (== #t q))
      '(#t))

;; Page 99
(test ">1o"
      (run* (q)
        (>1o '(1))
        (== #t q))
      '())

;; Page 99
(test ">1o"
      (run* (q)
        (>1o '())
        (== #t q))
      '())

;; Page 99
(test ">1o"
      (run* (r)
        (>1o r))
      '((_.0 _.1 . _.2)))

;; Page 100
(test "addero"
      (run 3 (s)
        (fresh (x y r)
          (addero 0 x y r)
          (== `(,x ,y ,r) s)))
      '((_.0 () _.0)
        (() (_.0 . _.1) (_.0 . _.1))
        ((1) (1) (0 1))))

;; Page  101
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
        ((1 1) (0 1) (1 0 1))
        ((1) (1 1 0 _.0 . _.1) (0 0 1 _.0 . _.1))
        ((1 0 _.0 . _.1) (1) (0 1 _.0 . _.1))
        ((1) (1 1 1 1) (0 0 0 0 1))
        ((0 1) (0 0 _.0 . _.1) (0 1 _.0 . _.1))
        ((1) (1 1 1 0 _.0 . _.1) (0 0 0 1 _.0 . _.1))
        ((1 1 1) (1) (0 0 0 1))
        ((1) (1 1 1 1 1) (0 0 0 0 0 1))
        ((0 1) (1 1) (1 0 1))
        ((1) (1 1 1 1 0 _.0 . _.1) (0 0 0 0 1 _.0 . _.1))
        ((1 1 0 _.0 . _.1) (1) (0 0 1 _.0 . _.1))
        ((1) (1 1 1 1 1 1) (0 0 0 0 0 0 1))))

;; Page 104
(define addero
  (lambda (d n m r)
    (condi
     ((== 0 d) (== '() m) (== n r))
     ((== 0 d) (== '() n) (== m r) (poso m))
     ((== 1 d) (== '() m) (addero 0 n '(1) r))
     ((== 1 d) (== '() n) (poso m) (addero 0 '(1) m r))
     ((== '(1) n) (== '(1) m) (fresh (a c)
                                (== `(,a ,c) r)
                                (full-addero d 1 1 a c)))
     ((== '(1) n) (gen-addero d n m r))
     ((== '(1) m) (>1o n) (>1o r) (addero d '(1) n r))
     ((>1o n) (gen-addero d n m r))
     (else fail))))

;; Page 104
(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
      (== `(,a . ,x) n)
      (== `(,b . ,y) m)
      (poso y)
      (== `(,c . ,z) r)
      (poso z)
      (alli
       (full-addero d a b c e)
       (addero e x y z)))))

;; Page 105
(test "addero"
      (run* (s)
        (fresh (x y)
          (addero 0 x y '(1 0 1))
          (== `(,x ,y) s)))
      '(((1 0 1) ())
        (() (1 0 1))
        ((1) (0 0 1))
        ((0 0 1) (1))
        ((1 1) (0 1))
        ((0 1) (1 1))))

;; +o: [bl] [bl] [bl] -> [goal]
;; Page 106
(define +o
  (lambda (n m k)
    (addero 0 n m k)))

;; Page 106
(test "+o"
      (run* (s)
        (fresh (x y)
          (+o x y '(1 0 1))
          (== `(,x ,y) s)))
      '(((1 0 1) ())
        (() (1 0 1))
        ((1) (0 0 1))
        ((0 0 1) (1))
        ((1 1) (0 1))
        ((0 1) (1 1))))

;; -o: [bl] [bl] [bl] -> [goal]
;; Page 106
(define -o
  (lambda (n m k)
    (+o m k n)))

;; Page 106
(test "-o"
      (run* (q)
        (-o '(0 0 0 1) '(1 0 1) q))
      '((1 1)))

;; Page 106
(test "-o"
      (run* (q)
        (-o '(0 1 1) '(0 1 1) q))
      '(()))

;; Page 106
(test "-o"
      (run* (q)
        (-o '(0 1 1) '(0 0 0 1) q))
      '())
