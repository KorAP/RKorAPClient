% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KorAPQuery.R
\docType{class}
\name{KorAPQuery-class}
\alias{KorAPQuery-class}
\alias{KorAPQuery}
\alias{initialize,KorAPQuery-method}
\alias{corpusQuery,KorAPConnection-method}
\alias{corpusQuery}
\alias{fetchNext,KorAPQuery-method}
\alias{fetchNext}
\alias{fetchAll,KorAPQuery-method}
\alias{fetchAll}
\alias{fetchRest,KorAPQuery-method}
\alias{fetchRest}
\alias{buildWebUIRequestUrlFromString}
\alias{buildWebUIRequestUrl}
\alias{format.KorAPQuery}
\alias{show,KorAPQuery-method}
\title{Class KorAPQuery}
\usage{
\S4method{initialize}{KorAPQuery}(
  .Object,
  korapConnection = NULL,
  request = NULL,
  vc = "",
  totalResults = 0,
  nextStartIndex = 0,
  fields = c("corpusSigle", "textSigle", "pubDate", "pubPlace", "availability",
    "textClass", "snippet", "tokens"),
  requestUrl = "",
  webUIRequestUrl = "",
  apiResponse = NULL,
  hasMoreMatches = FALSE,
  collectedMatches = NULL
)

\S4method{corpusQuery}{KorAPConnection}(
  kco,
  query = if (missing(KorAPUrl)) {
    
    stop("At least one of the parameters query and KorAPUrl must be specified.", call. =
    FALSE)
 } else {
     httr2::url_parse(KorAPUrl)$query$q
 },
  vc = if (missing(KorAPUrl)) "" else httr2::url_parse(KorAPUrl)$query$cq,
  KorAPUrl,
  metadataOnly = TRUE,
  ql = if (missing(KorAPUrl)) "poliqarp" else httr2::url_parse(KorAPUrl)$query$ql,
  fields = c("corpusSigle", "textSigle", "pubDate", "pubPlace", "availability",
    "textClass", "snippet", "tokens"),
  accessRewriteFatal = TRUE,
  verbose = kco@verbose,
  expand = length(vc) != length(query),
  as.df = FALSE,
  context = NULL
)

\S4method{fetchNext}{KorAPQuery}(
  kqo,
  offset = kqo@nextStartIndex,
  maxFetch = maxResultsPerPage,
  verbose = kqo@korapConnection@verbose,
  randomizePageOrder = FALSE
)

\S4method{fetchAll}{KorAPQuery}(kqo, verbose = kqo@korapConnection@verbose, ...)

\S4method{fetchRest}{KorAPQuery}(kqo, verbose = kqo@korapConnection@verbose, ...)

buildWebUIRequestUrlFromString(KorAPUrl, query, vc = "", ql = "poliqarp")

buildWebUIRequestUrl(
  kco,
  query = if (missing(KorAPUrl)) {
    
    stop("At least one of the parameters query and KorAPUrl must be specified.", call. =
    FALSE)
 } else {
     httr2::url_parse(KorAPUrl)$query$q
 },
  vc = if (missing(KorAPUrl)) "" else httr2::url_parse(KorAPUrl)$query$cq,
  KorAPUrl,
  ql = if (missing(KorAPUrl)) "poliqarp" else httr2::url_parse(KorAPUrl)$query$ql
)

\method{format}{KorAPQuery}(x, ...)

\S4method{show}{KorAPQuery}(object)
}
\arguments{
\item{.Object}{…}

\item{korapConnection}{KorAPConnection object}

\item{request}{query part of the request URL}

\item{vc}{string describing the virtual corpus in which the query should be performed. An empty string (default) means the whole corpus, as far as it is license-wise accessible.}

\item{totalResults}{number of hits the query has yielded}

\item{nextStartIndex}{at what index to start the next fetch of query results}

\item{fields}{(meta)data fields that will be fetched for every match.}

\item{requestUrl}{complete URL of the API request}

\item{webUIRequestUrl}{URL of a web frontend request corresponding to the API request}

\item{apiResponse}{data-frame representation of the JSON response of the API request}

\item{hasMoreMatches}{logical that signals if more query results can be fetched}

\item{collectedMatches}{matches already fetched from the KorAP-API-server}

\item{kco}{\code{\link[=KorAPConnection]{KorAPConnection()}} object (obtained e.g. from \code{KorAPConnection()}}

\item{query}{string that contains the corpus query. The query language depends on the \code{ql} parameter. Either \code{query} must be provided or \code{KorAPUrl}.}

\item{KorAPUrl}{instead of providing the query and vc string parameters, you can also simply copy a KorAP query URL from your browser and use it here (and in \code{KorAPConnection}) to provide all necessary information for the query.}

\item{metadataOnly}{logical that determines whether queries should return only metadata without any snippets. This can also be useful to prevent access rewrites. Note that the default value is TRUE.
If you want your corpus queries to return not only metadata, but also KWICS, you need to authorize
your RKorAPClient application as explained in the
\href{https://github.com/KorAP/RKorAPClient#authorization}{authorization section}
of the RKorAPClient Readme on GitHub and set the \code{metadataOnly} parameter to
\code{FALSE}.}

\item{ql}{string to choose the query language (see \href{https://github.com/KorAP/Kustvakt/wiki/Service:-Search-GET#user-content-parameters}{section on Query Parameters} in the Kustvakt-Wiki for possible values.}

\item{accessRewriteFatal}{abort if query or given vc had to be rewritten due to insufficient rights (not yet implemented).}

\item{verbose}{print progress information if true}

\item{expand}{logical that decides if \code{query} and \code{vc} parameters are expanded to all of their combinations. Defaults to \code{TRUE}, iff \code{query} and \code{vc} have different lengths}

\item{as.df}{return result as data frame instead of as S4 object?}

\item{context}{string that specifies the size of the left and the right context returned in \code{snippet}
(provided that \code{metadataOnly} is set to \code{false} and that the necessary access right are  met).
The format of the context size specifcation (e.g. \verb{3-token,3-token}) is described in the \href{https://github.com/KorAP/Kustvakt/wiki/Service:-Search-GET}{Service: Search GET documentation of the Kustvakt Wiki}.
If the parameter is not set, the default context size secification of the KorAP server instance will be used.
Note that you cannot overrule the maximum context size set in the KorAP server instance,
as this is typically legally motivated.}

\item{kqo}{object obtained from \code{\link[=corpusQuery]{corpusQuery()}}}

\item{offset}{start offset for query results to fetch}

\item{maxFetch}{maximum number of query results to fetch}

\item{randomizePageOrder}{fetch result pages in pseudo random order if true. Use \code{\link[=set.seed]{set.seed()}} to set seed for reproducible results.}

\item{...}{further arguments passed to or from other methods}

\item{x}{KorAPQuery object}

\item{object}{KorAPQuery object}
}
\value{
Depending on the \code{as.df} parameter, a tibble or a \code{\link[=KorAPQuery]{KorAPQuery()}} object that, among other information, contains the total number of results in \verb{@totalResults}. The resulting object can be used to fetch all query results (with \code{\link[=fetchAll]{fetchAll()}}) or the next page of results (with \code{\link[=fetchNext]{fetchNext()}}).
A corresponding URL to be used within a web browser is contained in \verb{@webUIRequestUrl}
Please make sure to check \verb{$collection$rewrites} to see if any unforeseen access rewrites of the query's virtual corpus had to be performed.

The \code{kqo} input object with updated slots \code{collectedMatches}, \code{apiResponse}, \code{nextStartIndex}, \code{hasMoreMatches}
}
\description{
This class provides methods to perform different kinds of queries on the KorAP API server.
\code{KorAPQuery} objects, which are typically created by the \code{\link[=corpusQuery]{corpusQuery()}} method,
represent the current state of a query to a KorAP server.

\strong{\code{corpusQuery}} performs a corpus query via a connection to a KorAP-API-server

\strong{\code{fetchNext}} fetches the next bunch of results of a KorAP query.

\strong{\code{fetchAll}} fetches all results of a KorAP query.
}
\examples{
\dontrun{

# Fetch metadata of every query hit for "Ameisenplage" and show a summary
KorAPConnection() \%>\%
  corpusQuery("Ameisenplage") \%>\%
  fetchAll()
}

\dontrun{

# Use the copy of a KorAP-web-frontend URL for an API query of "Ameise" in a virtual corpus
# and show the number of query hits (but don't fetch them).

KorAPConnection(verbose = TRUE) \%>\%
  corpusQuery(
    KorAPUrl =
      "https://korap.ids-mannheim.de/?q=Ameise&cq=pubDate+since+2017&ql=poliqarp"
  )
}

\dontrun{

# Plot the time/frequency curve of "Ameisenplage"
KorAPConnection(verbose = TRUE) \%>\%
  {
    . ->> kco
  } \%>\%
  corpusQuery("Ameisenplage") \%>\%
  fetchAll() \%>\%
  slot("collectedMatches") \%>\%
  mutate(year = lubridate::year(pubDate)) \%>\%
  dplyr::select(year) \%>\%
  group_by(year) \%>\%
  summarise(Count = dplyr::n()) \%>\%
  mutate(Freq = mapply(function(f, y) {
    f / corpusStats(kco, paste("pubDate in", y))@tokens
  }, Count, year)) \%>\%
  dplyr::select(-Count) \%>\%
  complete(year = min(year):max(year), fill = list(Freq = 0)) \%>\%
  plot(type = "l")
}
\dontrun{

q <- KorAPConnection() \%>\%
  corpusQuery("Ameisenplage") \%>\%
  fetchNext()
q@collectedMatches
}

\dontrun{

q <- KorAPConnection() \%>\%
  corpusQuery("Ameisenplage") \%>\%
  fetchAll()
q@collectedMatches
}

\dontrun{

q <- KorAPConnection() \%>\%
  corpusQuery("Ameisenplage") \%>\%
  fetchRest()
q@collectedMatches
}

}
\references{
\url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}

\url{https://ids-pub.bsz-bw.de/frontdoor/index/index/docId/9026}
}
\seealso{
\code{\link[=KorAPConnection]{KorAPConnection()}}, \code{\link[=fetchNext]{fetchNext()}}, \code{\link[=fetchRest]{fetchRest()}}, \code{\link[=fetchAll]{fetchAll()}}, \code{\link[=corpusStats]{corpusStats()}}
}
