#lang info

(define collection "package-analysis")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "package-analysis")))

(define deps
  (list "base"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
