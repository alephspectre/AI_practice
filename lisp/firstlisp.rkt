#lang racket
(define double
  (lambda (x) (+ x x)))
(define (double2 x)
  (* x 2))
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(struct node (key val left right))

(define (my-map func x)
  (my-map-helper func x null))

(define (my-map-helper func inlist outlist)
  (if (null? (cdr inlist))
      outlist
      (append outlist (list (func inlist)))))

(define (my-filter func x)
  (my-filter-helper func x null))

(define (my-filter-helper func mainlist finlist)
  (if (null? (cdr mainlist))
      finlist
      (if (func (car mainlist))
         (my-filter-helper func (cdr mainlist) (append finlist (list (car mainlist))))
         (my-filter-helper func (cdr mainlist) finlist))))

;;((lambda (f)(f 5)) (lambda (x) (+ x 5)))