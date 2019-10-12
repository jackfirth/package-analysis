#lang scribble/manual

@(require (for-label package-analysis
                     racket/base
                     racket/contract/base
                     racket/math))

@title{Package Versions}

@defproc[(package-version? [v any/c]) boolean?]

@defproc[(package-version [major natural?]
                          [minor (integer-in 0 99)]
                          [patch (integer-in 0 999) 0]
                          [release (integer-in 0 999) 0])
         package-version?]

@defproc[(package-version-major-part [version package-version?]) natural?]

@defproc[(package-version-minor-part [version package-version?])
         (integer-in 0 99)]

@defproc[(package-version-patch-part [version package-version?])
         (integer-in 0 999)]

@defproc[(package-version-release-part [version package-version?])
         (integer-in 0 999)]
