#lang info

(define collection "package-analysis")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "package-analysis")))

(define deps
  (list "base"
        "rebellion"))

(define build-deps
  (list "net-doc"
        "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
