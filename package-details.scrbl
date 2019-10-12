#lang scribble/manual

@(require (for-label package-analysis
                     racket/base
                     racket/contract/base
                     racket/math
                     racket/set
                     rebellion/base/immutable-string))

@title{Package Details}

@defproc[(package-details? [v any/c]) boolean?]

@defproc[(package-details
          [#:name name immutable-string?]
          [#:source source immutable-string?]
          [#:checksum checksum immutable-string?]
          [#:authors authors (set/c immutable-string?) (set)]
          [#:description description immutable-string? ""]
          [#:tags tags (set/c immutable-string?) (set)]
          [#:dependencies deps (set/c package-dependency?) (set)]
          [#:modules modules (set/c module-path?) (set)]
          [#:ring ring (or/c natural? #f) #f])
         package-details?]

@defproc[(package-details-name [package package-details?]) immutable-string?]

@defproc[(package-details-source [package package-details?]) immutable-string?]

@defproc[(package-details-checksum [package package-details?])
         immutable-string?]

@defproc[(package-details-authors [package package-details?])
         (set/c immutable-string?)]

@defproc[(package-details-description [package package-details?])
         immutable-string?]

@defproc[(package-details-tags [package package-details?])
         (set/c immutable-string?)]

@defproc[(package-details-dependencies [package package-details?])
         (set/c package-dependency?)]

@defproc[(package-details-modules [package package-details?])
         (set/c module-path?)]

@defproc[(package-details-ring [package package-details?])
         (or/c natural? #f)]
