#lang scribble/manual

@(require (for-label net/url
                     package-analysis
                     racket/base
                     racket/contract/base))

@title{Package Catalog Clients}

A @deftech{package catalog client}, or simply @deftech{package client}, is an
object used to connect to a package catalog. Clients encapsulate state
associated with the catalog such as caches and network connections. The @racket[
 current-package-client] parameter provides a client to all of the functions in
@racketmodname[package-analysis]. By default, this parameter is set to a client
for the official package catalog at @hyperlink["https://pkgs.racket-lang.org/"]{
 pkgs.racket-lang.org}.

@defproc[(package-client? [v any/c]) boolean?]{
 A predicate for @tech{package clients}.}

@defparam[current-package-client client package-client?
          #:value (make-package-client
                   (string->url "https://pkgs.racket-lang.org"))]{
 A parameter that defines the @tech{package client} used by @racketmodname[
 package-analysis]. By default, a client for the official package catalog is
 used.}

@defproc[(make-package-client [catalog url?]) package-client?]{
 Constructs a new @tech{package client} for @racket[catalog].}
