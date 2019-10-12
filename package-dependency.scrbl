#lang scribble/manual

@(require (for-label package-analysis
                     racket/base
                     racket/contract/base
                     rebellion/base/immutable-string
                     rebellion/base/symbol))

@title{Package Dependencies}

@defproc[(package-dependency? [v any/c]) boolean?]

@defproc[(package-dependency
          [#:source source immutable-string?]
          [#:lower-version-bound version package-version? (package-version 0 0)]
          [#:platform-requirement platform (or/c platform-requirement? #f) #f])
         package-dependency?]

@defproc[(package-dependency-source [dep package-dependency?])
         immutable-string?]

@defproc[(package-dependency-lower-version-bound [dep package-dependency?])
         package-version?]

@defproc[(package-dependency-platform-requirement [dep package-dependency?])
         (or/c platform-requirement? #f)]

@defproc[(platform-requirement? [v any/c]) boolean?]

@defproc[(system-type-requirement? [v any/c]) boolean?]

@defproc[(system-type-requirement [type interned-symbol?])
         system-type-requirement?]

@defproc[(system-type-requirement-value [requirement system-type-requirement?])
         interned-symbol?]

@defproc[(system-library-subpath-requirement? [v any/c]) boolean?]

@defproc[(system-library-subpath-requirement
          [path (or/c immutable-string? regexp?)])
         system-library-subpath-requirement?]

@defproc[(system-library-subpath-requirement-value
          [requirement system-library-subpath-requirement?])
         (or/c immutable-string? regexp?)]
