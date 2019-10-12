#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [package-details
   (->* (#:source immutable-string?
         #:checksum immutable-string?
         #:name immutable-string?)
        (#:authors (immutable-set/c immutable-string?)
         #:description immutable-string?
         #:tags (immutable-set/c immutable-string?)
         #:dependencies (immutable-set/c package-dependency?)
         #:modules (immutable-set/c module-path?)
         #:ring (or/c natural? #f))
        package-details?)]
  [package-details? predicate/c]
  [package-details-source (-> package-details? immutable-string?)]
  [package-details-checksum (-> package-details? immutable-string?)]
  [package-details-name (-> package-details? immutable-string?)]
  [package-details-authors
   (-> package-details? (immutable-set/c immutable-string?))]
  [package-details-description (-> package-details? immutable-string?)]
  [package-details-tags
   (-> package-details? (immutable-set/c immutable-string?))]
  [package-details-dependencies
   (-> package-details? (immutable-set/c package-dependency?))]
  [package-details-modules
   (-> package-details? (immutable-set/c module-path?))]
  [package-details-ring (-> package-details? (or/c natural? #f))]
  [package-dependency
   (->* (#:source immutable-string?)
        (#:lower-version-bound package-version?
         #:platform-requirement (or/c platform-requirement? #f))
        package-dependency?)]
  [package-dependency? predicate/c]
  [package-dependency-source (-> package-dependency? immutable-string?)]
  [package-dependency-lower-version-bound
   (-> package-dependency? package-version?)]
  [package-dependency-platform-requirement
   (-> package-dependency? platform-requirement?)]
  [package-version
   (->* (natural? (integer-in 0 99)) ((integer-in 0 999) (integer-in 0 999))
        package-version?)]
  [package-version? predicate/c]
  [package-version-major-part (-> package-version? natural?)]
  [package-version-minor-part (-> package-version? (integer-in 0 99))]
  [package-version-patch-part (-> package-version? (integer-in 0 999))]
  [package-version-release-part (-> package-version? (integer-in 0 999))]
  [platform-requirement? predicate/c]
  [system-type-requirement
   (-> interned-symbol? system-type-requirement?)]
  [system-type-requirement? predicate/c]
  [system-type-requirement-value (-> system-type-requirement? interned-symbol?)]
  [system-library-subpath-requirement
   (-> (or/c immutable-string? regexp?) system-library-subpath-requirement?)]
  [system-library-subpath-requirement? predicate/c]
  [system-library-subpath-requirement-value
   (-> system-library-subpath-requirement? (or/c immutable-string? regexp?))]))

(require racket/math
         racket/set
         rebellion/base/immutable-string
         rebellion/base/symbol
         rebellion/type/record
         rebellion/type/tuple
         rebellion/type/wrapper)

;@------------------------------------------------------------------------------

(define (immutable-set/c contract)
  (set/c contract #:cmp 'equal))

(define-tuple-type package-version
  (major-part minor-part patch-part release-part)
  #:constructor-name constructor:package-version)

(define (package-version major minor [patch 0] [release 0])
  (constructor:package-version major minor patch release))

(define-wrapper-type system-type-requirement)
(define-wrapper-type system-library-subpath-requirement)

(define (platform-requirement? v)
  (or (system-type-requirement? v) (system-library-subpath-requirement? v)))

(define-record-type package-dependency
  (source lower-version-bound platform-requirement)
  #:constructor-name constructor:package-dependency)

(define (package-dependency
         #:source source
         #:lower-version-bound [lower-version-bound (package-version 0 0)]
         #:platform-requirement [platform-requirement #f])
  (constructor:package-dependency
   #:source source
   #:lower-version-bound lower-version-bound
   #:platform-requirement platform-requirement))

(define-record-type package-details
  (source
   checksum
   name
   authors
   description
   tags
   dependencies
   modules
   ring)
  #:constructor-name constructor:package-details)

(define (package-details #:source source
                         #:checksum checksum
                         #:name name
                         #:authors [authors (set)]
                         #:description [description ""]
                         #:tags [tags (set)]
                         #:dependencies [dependencies (set)]
                         #:modules [modules (set)]
                         #:ring [ring #f])
  (constructor:package-details
   #:source source
   #:checksum checksum
   #:name name
   #:authors authors
   #:description description
   #:tags tags
   #:dependencies dependencies
   #:modules modules
   #:ring ring))
