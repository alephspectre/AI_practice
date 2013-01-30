#lang racket
(define (my-last x)
  (if (null? (cdr x))
      (car x)
      (my-last (cdr x))))

(define (my-but-last x)
  (if (null? (cddr x))
      (car x)
      (my-but-last (cdr x))))

(define (element-at x n)
  (if (or (= n 0) (null? (cdr x)))
      (car x)
      (element-at (cdr x) (- n 1))))

(define (list-length-help x n)
  (if (null? (cdr x))
      n
      (list-length-help (cdr x) (+ n 1))))

(define (list-length x)
  (list-length-help x 1))

(define (my-map func x)
  (my-map-helper func inlist outlist))

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