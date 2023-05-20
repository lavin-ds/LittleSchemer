#lang racket
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (lat? x)
  (cond
    ((null? x) #t)
    ((atom? (car x))(lat? (cdr x)))
    (else #f)))

(define (member? x lat)
  (cond
    ((null? lat) #f)
    (else
     (or (eq? (car lat) x)
         (member? x (cdr lat))))))
                    
(define (rember x lat)
  (cond
    ((null? lat) (quote()))
    ((eq? (car lat) x) (cdr lat))
    (else (cons (car lat) (rember x (cdr lat))))))

(define (firsts x)
  (cond
    ((null? x)(quote()))
    (else
     (cons (caar x) (firsts (cdr x))))))

(define (insertR new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? (car lat) old) (cons old (cons new (cdr lat))))
    (else
     (cons (car lat) (insertR new old (cdr lat))))))

(define (insertL new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? (car lat) old) (cons new lat))
    (else
     (cons (car lat) (insertL new old (cdr lat))))))

(define (subst new old lat)
  (cond
    ((null? lat)(quote()))
    ((eq? (car lat) old) (cons new (cdr lat)))
    (else
     (cons (car lat) (subst new old (cdr lat))))))

(define (subst2 new o1 o2 lat)
  (cond
    ((null? lat)(quote()))
    (else
     (cond
      ((or
        (eq? (car lat) o1)
        (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else
       (cons (car lat) (subst2 new o1 o2 (cdr lat))))))))

(define (multirember x lat)
  (cond
    ((null? lat) (quote()))
    ((eq? (car lat) x) (multirember x (cdr lat)))
    (else
     (cons (car lat) (multirember x (cdr lat))))))

(define (multiinsertR new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
    (else
     (cons (car lat) (multiinsertR new old (cdr lat))))))

(define (multiinsertL new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
    (else
     (cons (car lat) (multiinsertL new old (cdr lat))))))

(define (multisubst new old lat)
  (cond
    ((null? lat) (quote()))
    ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
    (else
     (cons (car lat) (multisubst new old (cdr lat))))))

(define (add1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

(define (+. x y)
    (cond
      ((zero? y) x)
      (else
       (add1 (+. x (sub1 y))))))


(define (-. x y)
  (cond
    ((zero? y) x)
    (else
     (sub1 (-. x (sub1 y))))))
  
(define (tup? l)
  (cond
    ((null? l) #t)
    (else
     (and (number? (car l))
         (tup? (cdr l))))))

(define (addtup tup)
  (cond
    ((null? tup) 0)
    (else
     (+. (car tup) (addtup (cdr tup))))))

(define (*. a b )
  (cond
    ((zero? b) 0)
    (else
     (+. a (*. a (sub1 b))))))

(define (tup+ tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else
     (cons (+. (car tup1) (car tup2))
           (tup+ (cdr tup1) (cdr tup2))))))

(define (>. x y)
  (cond
    ((zero? x) #f)
    ((zero? y) #t)
    (else
     (>. (sub1 x) (sub1 y)))))

(define (<. x y)
  (cond
    ((zero? y) #f)
    ((zero? x) #t)
    (else
     (<. (sub1 x) (sub1 y)))))

(define (=. x y)
  (cond
    ((zero? x) (zero? y))
    ((zero? x) #f)
    (else
     (=. (sub1 x) (sub1 y)))))

(define (=.. x y)
  (cond
    ((>. x y) #f)
    ((<. x y) #f)
    (else #t)))

(define (^. x y)
  (cond
    ((zero? y) 1)
    (else
     (*. x (^. x (sub1 y))))))

(define (/. x y )
  (cond
    ((<. x y ) 0)
    (else
     (add1 (/. (-. x y) y)))))

(define (length lat)
  (cond
    ((null? lat) 0)
    (else
     (add1 (length (cdr lat))))))

(define (pick n lat)
  (cond
    ((zero? (sub1 n)) (car lat))
    (else
     (pick (sub1 n) (cdr lat)))))

(define (rempick n lat)
  (cond
    ((zero? (sub1 n)) (cdr lat))
    (else
     (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(define (no-nums lat)
  (cond
    ((null? lat) (quote()))
    ((number? (car lat)) (no-nums (cdr lat)))
    (else
     (cons (car lat) (no-nums (cdr lat))))))


(define (all-nums lat)
  (cond
    ((null? lat) (quote()))
    ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
    (else
     (all-nums (cdr lat)))))

(define (eqan? x y)
  (cond
    ((and (number? x)(number? y)) (= x y))
    ((or (number? x)(number? y)) #f)
    (else
     (eq? x y))))

(define (occur a lat)
  (cond
    ((null? lat) 0)
    ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
    (else
     (occur a (cdr lat)))))

(define (one? x)
    (and (number? x) (= 1 x)))

(define (rempick- n lat)
  (cond
    ((one? n) (cdr lat))
    (else
     (cons (car lat) (rempick (sub1 n) (cdr lat))))))
