% Please edit documentation in R/KorAPQuery.R if changing API
\name{fetchAnnotations}
\alias{fetchAnnotations}
\title{Fetch annotations (generic)}
\description{
S4 generic for fetching token annotations for collected matches. See specific methods, e.g. \code{\link{fetchAnnotations,KorAPQuery-method}}.
}
\usage{
fetchAnnotations(kqo, foundry = "tt", overwrite = FALSE,
  verbose = kqo@korapConnection@verbose)
}
\arguments{
\item{kqo}{An object on which an annotation fetching method is defined.}
\item{foundry}{Annotation foundry identifier.}
\item{overwrite}{Logical flag controlling whether to overwrite existing annotations.}
\item{verbose}{Logical flag for progress output.}
}
\seealso{
\code{\link{fetchAnnotations,KorAPQuery-method}}
}
