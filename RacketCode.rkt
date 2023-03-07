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
