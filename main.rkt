#lang racket/base

(provide
 (all-from-out package-analysis/data-model
               package-analysis/client))

(require package-analysis/data-model
         package-analysis/client
         racket/list
         racket/set
         rebellion/collection/entry
         rebellion/collection/multidict
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define (get-authorship-graph)
  (for*/multidict ([pkg (in-immutable-set (get-all-packages))]
                   [author (in-immutable-set (package-details-authors pkg))])
    (entry author (package-details-name pkg))))

(define (get-tag-graph)
  (for*/multidict ([pkg (in-immutable-set (get-all-packages))]
                   [tag (in-immutable-set (package-details-tags pkg))])
    (entry tag (package-details-name pkg))))

(define (get-dependency-graph)
  (for*/multidict ([pkg (in-immutable-set (get-all-packages))]
                   [dep (in-immutable-set (package-details-dependencies pkg))])
    (entry (package-details-name pkg)
           (package-dependency-source dep))))

(define (get-clients pkg)
  (multidict-ref (multidict-inverse (get-dependency-graph)) pkg))

(define (get-rebellion-clients)
  (multidict-ref (multidict-inverse (get-dependency-graph)) "rebellion"))

(define (depends-on? details pkg)
  (for/or ([dep (in-list (hash-ref details 'dependencies))])
    (equal? dep pkg)))

(define (get-transitive-deps pkg #:graph [deps (get-dependency-graph)])
  (let loop ([unanalyzed-packages (set pkg)]
             [analyzed-packages (set)]
             [known-dependencies (set)])
    (cond
      [(set-empty? unanalyzed-packages) known-dependencies]
      [else
       (define next (set-first unanalyzed-packages))
       (define rest (set-rest unanalyzed-packages))
       (define new-direct-deps
         (set-subtract (multidict-ref deps next)
                       analyzed-packages
                       known-dependencies))
       (loop (set-union rest new-direct-deps)
             (set-add analyzed-packages next)
             (set-union known-dependencies new-direct-deps))])))

(define-record-type package-statistics
  (package
   direct-dependency-count
   direct-client-count
   transitive-dependency-count
   transitive-client-count))

(define-record-type package-relationships
  (package
   direct-dependencies
   direct-clients
   transitive-dependencies
   transitive-clients))

(define (get-package-relationships
         pkg
         #:dep-graph [graph (get-dependency-graph)]
         #:reverse-dep-graph [reverse-graph (multidict-inverse graph)])
  (define direct-deps (multidict-ref graph pkg))
  (define direct-clients (multidict-ref reverse-graph pkg))
  (define trans-deps (get-transitive-deps pkg #:graph graph))
  (define trans-clients
    (get-transitive-deps pkg #:graph reverse-graph))
  (package-relationships
   #:package pkg
   #:direct-dependencies direct-deps
   #:direct-clients direct-clients
   #:transitive-dependencies trans-deps
   #:transitive-clients trans-clients))

(define (get-package-statistics
         pkg
         #:dep-graph [graph (get-dependency-graph)]
         #:reverse-dep-graph [reverse-graph (multidict-inverse graph)])
  (define rels
    (get-package-relationships pkg
                               #:dep-graph graph
                               #:reverse-dep-graph reverse-graph))
  (package-statistics
   #:package pkg
   #:direct-dependency-count
   (set-count (package-relationships-direct-dependencies rels))
   #:direct-client-count
   (set-count (package-relationships-direct-clients rels))
   #:transitive-dependency-count
   (set-count (package-relationships-transitive-dependencies rels))
   #:transitive-client-count
   (set-count (package-relationships-transitive-clients rels))))

(define (top-packages-by-transitive-dep-count)
  (define deps (get-dependency-graph))
  (define reverse-deps (multidict-inverse deps))
  (define all-pkg-stats
    (for/list ([pkg (in-immutable-set (multidict-unique-keys deps))])
      (get-package-statistics pkg
                              #:dep-graph deps
                              #:reverse-dep-graph reverse-deps)))
  (define sorted-pkg-stats
    (sort all-pkg-stats > #:key package-statistics-transitive-dependency-count))
  (take sorted-pkg-stats 20))
