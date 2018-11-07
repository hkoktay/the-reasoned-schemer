;; Chapter 9: Under the Hood

(load "test-check.scm")
(load "minikanren/mkprelude.scm")

;; Page 131
(define u (var 'u))
(define v (var 'v))
(define w (var 'w))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

;; Page 132
(define empty-s '())

;; Page 133
(test "walk"
      (walk z `((,z . a) (,x . ,w) (,y . ,z)))
      'a)

;; Page 133
(test "walk"
      (walk y `((,z . a) (,x . ,w) (,y . ,z)))
      'a)

;; The result is #(0) because 'var' is implemented as a vector.
;; Page 133
(test "walk"
      (walk w `((,z . a) (,x . ,w) (,y . ,z)))
      '#(w))

;; Page 133
(test "walk"
      (walk w `((,x . ,y) (,w . b) (,z . ,x) (,y . ,z)))
      'b)

;; Page 134
(test "walk"
      (walk x `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
      'b)

;; Page 134
(test "walk"
      (walk u `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
      'b)

;; Page 134
(test "walk"
      (walk v `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
      'b)

;; Page 134
(test "walk"
      (walk w `((,y . b) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
      'b)

;; Page 134
(test "walk"
      (walk x `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
      '#(z))

;; Page 134
(test "walk"
      (walk u `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
      '#(z))

;; Page 134
(test "walk"
      (walk v `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
      '#(z))

;; Page 134
(test "walk"
      (walk w `((,y . ,z) (,x . ,y) (,v . ,x) (,w . ,x) (,u . ,w)))
      '#(z))

;; Page 134
(test "walk"
      (walk u `((,x . b) (,w . (,x e ,x)) (,u . ,w)))
      `(,x e ,x))

;; walk: [var] [substitution] -> [var] | any
;;
;; Returns either a fresh [var] or the value of the [var] found in the
;; substitution 's'.
(define walk
  (lambda (v s)
    (cond
     ((var? v)
      (let ((a (assq v s)))
        (cond
         (a ((lambda (a)
               (walk (rhs a) s)) a))
         (else v))))
     (else v))))

;; ext-s: [var] any [substitution] -> [substitution]
(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

;; Page 135
(test "ext-s"
      (ext-s x y `((z . ,x) (,y . ,z)))
      '((#(x) . #(y)) (z . #(x)) (#(y) . #(z))))

;; Page 135
(test "walk"
      (walk y `((,x . e)))
      '#(y))

;; Page 135
(test "ext-s"
      (ext-s y x `((,x . e)))
      '((#(y) . #(x)) (#(x) . e)))

;; Page 135
(test "walk and ext-s"
      (walk y (ext-s y x `((,x . e))))
      'e)

;; Page 135
(test "walk"
      (walk x `((,y . ,z) (,x . ,y)))
      '#(z))

;; Page 135
(test "walk and ext-s"
      (walk x (ext-s z 'b `((,y . ,z) (,x . ,y))))
      'b)

;; Page 135
(test "walk and ext-s"
      (walk x (ext-s z w `((,y . ,z) (,x . ,y))))
      '#(w))

;; unify: any any -> boolean | [substitution]
;; Page 135
(define unify
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
       ((eq? v w) s)
       ((var? v) (ext-s v w s))
       ((var? w) (ext-s w v s))
       ((and (pair? v) (pair? w))
        (let ((u (unify (car v) (car w) s)))
          (cond
           (u ((lambda (s) (unify (cdr v) (cdr w) s)) u))
           (else #f))))
       ((equal? v w) s)
       (else #f)))))

;; Page 137
(test "walk*"
      (walk* x `((,y . (a ,z c)) (,x . ,y) (,z . a)))
      '(a a c))

;; Page 137
(test "walk*"
      (walk* x `((,y . (,z ,w c)) (,x . ,y) (,z . a)))
      '(a #(w) c))

;; Page 138
(test "walk*"
      (walk* y `((,y . (,w ,z c)) (,v . b) (,x . ,v) (,z . ,x)))
      '(#(w) b c))

;; walk*: [var] [substitution] -> [var] | list | any
;; Page 138
(define walk*
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
           (walk* (car v) s)
           (walk* (cdr v) s)))
        (else v)))))

;; reify-s: any [substitution] -> [substitution]
;;
;; Returns a reified name substitution. Each variable v is associated with its
;; reified name.
;; 
;; Page 139
(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
       ((var? v)
        (ext-s v (reify-name (size-s s)) s))
       ((pair? v)
        (reify-s (cdr v) (reify-s (car v) s)))
       (else s)))))

;; reify-name: [number] -> string
;; Page 139
(define reify-name
  (lambda (n)
    (string->symbol
     (string-append "_" "." (number->string n)))))

;; Page 139
(test "reify-s"
      (let ((r `(,w ,x ,y)))
        (walk* r (reify-s r empty-s)))
      '(_.0 _.1 _.2))

;; Page 139
(test "reify-s"
      (let ((r (walk* `(,x ,y ,z) empty-s)))
        (walk* r (reify-s r empty-s)))
      '(_.0 _.1 _.2))

;; Page 139
(test "reify-s"
      (let ((r `(,u (,v (,w ,x) ,y) ,x)))
        (walk* r (reify-s r empty-s)))
      '(_.0 (_.1 (_.2 _.3) _.4) _.3))

;; Page 139
(test "reify-s"
      (let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))))
        (let ((r (walk* x s)))
          (walk* r (reify-s r empty-s))))
      '(a _.0 c _.0))

;; reify: any -> [substitution]
;; Page 140
(define reify
  (lambda (v)
    (walk* v (reify-s v empty-s))))

;; Page 140
(test "reify"
      (let ((s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))))
        (reify (walk* x s)))
      '(a _.0 c _.0))

;; ext-s-check: [var] any [substitution] -> [substitution]
;; Page 140
(define ext-s-check
  (lambda (x v s)
    (cond
     ((occurs-check x v s) #f)
     (else (ext-s x v s)))))

;; occurs-check: any any [substitution] -> boolean
;; Page 140
(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
       ((var? v) (eq? v x))
       ((pair? v)
        (or (occurs-check x (car v) s)
            (occurs-check x (cdr v) s)))
       (else fail)))))

;; unify-check: any any [substitution] -> [substitution]
;; Page 140
(define unify-check
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
       ((eq? v w) s)
       ((var? v) (ext-s-check v w s))
       ((var? w) (ext-s-check w v s))
       ((and (pair? v) (pair? w))
        (let ((u (unify-check (car v) (car w) s)))
          (cond
           (u ((lambda (s) (unify-check (cdr v) (cdr w) s)) u))
           (else #f))))
       ((equal? v w) s)
       (else #f)))))

;; Page 141
(test "circular substitution"
      (run 1 (q)
        (fresh (x)
          (== `(,x) x)
          (== #t q)))
      '(#t))

;; Page 141
(test "circular substitution"
      (run 1 (q)
        (fresh (x y)
          (== `(,x) y)
          (== `(,y) x)
          (== #t q)))
      '(#t))

;; ==-check: any any -> [goal]
;; Page 141
(define ==-check
  (lambda (v w)
    (lambda (s)
      (cond
        ((unify-check v w s) => succeed)
        (else (fail s))))))

;; Page 142
(test "==-check"
      (run 1 (x)
        (fresh (y z)
          (== x z)
          (== `(a b ,z) y)
          (==-check x y)))
      '())

;; Page 142
(test "==-check"
      (run 1 (x)
        (fresh (y z)
          (== x z)
          (== `(a b ,z) y)
          (==-check x y)))
      '())
