#lang scribble/manual

@(require (for-label package-analysis))

@title{Package Analysis}
@defmodule[package-analysis]

The @racketmodname[package-analysis] package provides APIs to query package
catalogs for information about packages. However, documentation and features are
sparse for the time being.

@table-of-contents[]

@include-section[(lib "package-analysis/catalog.scrbl")]
@include-section[(lib "package-analysis/client.scrbl")]
@include-section[(lib "package-analysis/package-details.scrbl")]
@include-section[(lib "package-analysis/package-dependency.scrbl")]
@include-section[(lib "package-analysis/package-version.scrbl")]
