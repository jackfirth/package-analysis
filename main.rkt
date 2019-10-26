#lang racket/base

(provide
 (all-from-out package-analysis/data-model
               package-analysis/client))

(require package-analysis/data-model
         package-analysis/client
         racket/set
         rebellion/base/comparator
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/multidict
         rebellion/streaming/transducer
         rebellion/type/record)

;@------------------------------------------------------------------------------

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
         pkg #:client [client (current-package-client)])
  (define dep-graph (get-dependency-graph #:client client))
  (define direct-deps (multidict-ref dep-graph pkg))
  (define direct-clients (multidict-ref (multidict-inverse dep-graph) pkg))
  (define trans-deps (get-transitive-dependencies pkg #:client client))
  (define trans-clients (get-transitive-clients pkg #:client client))
  (package-relationships
   #:package pkg
   #:direct-dependencies direct-deps
   #:direct-clients direct-clients
   #:transitive-dependencies trans-deps
   #:transitive-clients trans-clients))

(define (get-package-statistics pkg #:client [client (current-package-client)])
  (define rels (get-package-relationships pkg #:client client))
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
  (transduce (get-all-packages)
             (mapping package-details-name)
             (bisecting values get-transitive-dependencies)
             (mapping-values set-count)
             (sorting (comparator-reverse real<=>) #:key entry-value)
             (taking 10)
             #:into into-hash))
