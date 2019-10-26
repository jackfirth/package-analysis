#lang scribble/manual

@(require (for-label package-analysis
                     racket/base
                     racket/set
                     rebellion/base/immutable-string
                     rebellion/base/option
                     rebellion/collection/multidict))

@title{Querying Package Catalogs}

@defproc[(get-package
          [name immutable-string?]
          [#:client client package-client? (current-package-client)])
         (option/c package-details?)]{
 Looks up the package @racket[name] using @racket[client] and returns
 information about the package, or @racket[absent] if it doesn't exist.}

@defproc[(get-all-packages
          [#:client client package-client? (current-package-client)])
         (set/c package-details?)]{
 Returns all packages in the package catalog that @racket[client] connects to.}

@defproc[(get-dependency-graph
          [#:client client package-client? (current-package-client)])
         multidict?]{
 Returns all dependency relationships in the package catalog, in the form of a
 multidict mapping package names to the names of their dependencies.}

@defproc[(get-transitive-dependencies
          [pkg immutable-string?]
          [#:client client package-client? (current-package-client)])
         (set/c immutable-string?)]{
 Queries the package catalog with @racket[client] and returns a set of all
 dependencies of @racket[pkg], including both direct and indirect dependencies.}

@defproc[(get-transitive-clients
          [pkg immutable-string?]
          [#:client client package-client? (current-package-client)])
         (set/c immutable-string?)]{
 Queries the package catalog with @racket[client] and returns a set of all
 clients of @racket[pkg] --- that is, all packages that depend @emph{on}
 @racket[pkg] --- including both direct and indirect clients.}
