#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [package-client? predicate/c]
  [make-package-client (-> url? package-client?)]
  [current-package-client (parameter/c package-client?)]
  [get-package
   (->* (immutable-string?) (#:client package-client?)
        (option/c package-details?))]
  [get-all-packages
   (->* () (#:client package-client?) (immutable-set/c package-details?))]
  [get-dependency-graph
   (->* () (#:client package-client?) multidict?)]))

(require net/url
         package-analysis/data-model
         racket/bool
         racket/match
         racket/set
         racket/string
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/list
         rebellion/collection/multidict
         rebellion/type/record
         rebellion/type/reference)

;@------------------------------------------------------------------------------

(define (immutable-set/c contract) (set/c contract #:cmp 'equal))

(define-reference-type package-client
  (catalog all-packages-cache details-cache dependency-graph-cache)
  #:constructor-name constructor:package-client)

(define (make-package-client catalog)
  (constructor:package-client
   #:catalog catalog
   #:all-packages-cache (box #f)
   #:details-cache (make-hash)
   #:dependency-graph-cache (box #f)))

(define official-catalog (string->url "https://pkgs.racket-lang.org"))

(define current-package-client
  (make-parameter (make-package-client official-catalog)))

(define (hash->package-details details-hash)
  (define authors (string-split (hash-ref details-hash 'author "")))
  (define tags (hash-ref details-hash 'tags empty-list))
  (define raw-deps (hash-ref details-hash 'dependencies empty-list))
  (define modules (hash-ref details-hash 'modules empty-list))
  (package-details
   #:source (string->immutable-string (hash-ref details-hash 'source))
   #:checksum (string->immutable-string (hash-ref details-hash 'checksum))
   #:name (string->immutable-string (hash-ref details-hash 'name))
   #:authors
   (for/set ([author (in-list authors)]) (string->immutable-string author))
   #:description
   (string->immutable-string (hash-ref details-hash 'description ""))
   #:tags (for/set ([tag (in-list tags)]) (string->immutable-string tag))
   #:dependencies
   (for/set ([dep (in-list raw-deps)]) (raw-dependency->package-dependency dep))
   #:modules (list->set modules)
   #:ring (hash-ref details-hash 'ring #f)))

(define (raw-dependency->package-dependency raw-dep)
  (match raw-dep
    
    [(or (list source version)
         (list source '#:version version))
     (package-dependency
      #:source (string->immutable-string source)
      #:lower-version-bound (string->package-version version))]

    [(list source '#:platform platform)
     (package-dependency
      #:source (string->immutable-string source)
      #:platform-requirement
      (raw-platform-spec->platform-requirement platform))]
    
    [(or (list source '#:version version '#:platform platform)
         (list source '#:platform platform '#:version version))
     (package-dependency
      #:source (string->immutable-string source)
      #:lower-version-bound (string->package-version version)
      #:platform-requirement
      (raw-platform-spec->platform-requirement platform))]
    
    [(or (? string? source) (list source))
     (package-dependency #:source (string->immutable-string source))]))

(define (string->package-version version)
  (match (string-split version "." #:trim? #f)
    [(list major minor)
     (package-version (string->number major) (string->number minor))]
    [(list major minor patch)
     (package-version
      (string->number major) (string->number minor) (string->number patch))]
    [(list major minor patch release)
     (package-version (string->number major)
                      (string->number minor)
                      (string->number patch)
                      (string->number release))]))

(define (raw-platform-spec->platform-requirement platform-spec)
  (cond
    [(symbol? platform-spec) (system-type-requirement platform-spec)]
    [(string? platform-spec)
     (system-library-subpath-requirement
      (string->immutable-string platform-spec))]
    [(regexp? platform-spec)
     (system-library-subpath-requirement platform-spec)]))

(define (hash-ref-option h k)
 (if (hash-has-key? h k)
     (present (hash-ref h k))
     absent))

(define (package-client-load-all-packages! client)
  (define catalog (package-client-catalog client))
  (define all-packages-cache (package-client-all-packages-cache client))
  (when (false? (unbox all-packages-cache))
    (define all-packages
      (call/input-url (combine-url/relative catalog "./pkgs-all")
                      (λ (resource) (get-pure-port resource #:redirections 5))
                      read))
    (set-box! all-packages-cache all-packages)))

(define (package-client-load-package! client package)
  (define details-cache (package-client-details-cache client))
  (unless (hash-has-key? details-cache package)
    (package-client-load-all-packages! client)
    (define all-packages-cache (package-client-all-packages-cache client))
    (define cached-all-packages (unbox all-packages-cache))
    (define details
      (option-map (hash-ref-option cached-all-packages package)
                  hash->package-details))
    (hash-set! details-cache package details)))

(define (get-package name #:client [client (current-package-client)])
  (package-client-load-package! client name)
  (hash-ref (package-client-details-cache client) name))

(define (get-all-packages #:client [client (current-package-client)])
  (package-client-load-all-packages! client)
  (define all-packages
    (unbox (package-client-all-packages-cache client)))
  (for/set ([name (in-hash-keys all-packages)])
    (package-client-load-package! client name)
    (present-value (hash-ref (package-client-details-cache client) name))))

(define (package-client-load-dependency-graph! client)
  (define cache (package-client-dependency-graph-cache client))
  (when (false? (unbox cache))
    (define graph
      (for*/multidict
          ([pkg (in-immutable-set (get-all-packages #:client client))]
           [dep (in-immutable-set (package-details-dependencies pkg))])
        (entry (package-details-name pkg)
               (package-dependency-source dep))))
    (set-box! cache graph)))

(define (get-dependency-graph #:client [client (current-package-client)])
  (package-client-load-dependency-graph! client)
  (unbox (package-client-dependency-graph-cache client)))
