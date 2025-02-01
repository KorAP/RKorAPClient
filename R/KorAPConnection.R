################################################################################
# Use setClassUnion to define the unholy NULL-data union as a virtual class.
################################################################################
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
# setOldClass("httr2_oauth_client")

#' Class KorAPConnection
#'
#' `KorAPConnection` objects represent the connection to a KorAP server.
#' New `KorAPConnection` objects can be created by `new("KorAPConnection")`.
#'
#' @import R.cache
#' @import utils
#' @import methods
#' @slot KorAPUrl        URL of the web user interface of the KorAP server used in the connection.
#' @slot apiVersion      requested KorAP API version.
#' @slot indexRevision   indexRevision code as reported from API via `X-Index-Revision` HTTP header.
#' @slot apiUrl          full URL of API including version.
#' @slot accessToken     OAuth2 access token.
#' @slot oauthClient     OAuth2 client object.
#' @slot oauthScope      OAuth2 scope.
#' @slot userAgent       user agent string used for connection the API.
#' @slot timeout         timeout in seconds for API requests (this does not influence server internal timeouts)
#' @slot verbose         logical that decides whether operations will default to be verbose.
#' @slot cache           logical that decides if API calls are cached locally.
#' @slot welcome         list containing HTTP response received from KorAP server welcome function.

#' @export
KorAPConnection <- setClass("KorAPConnection", slots=c(KorAPUrl="character", apiVersion="character", indexRevision="characterOrNULL", apiUrl="character", accessToken="characterOrNULL", oauthClient="ANY", oauthScope="characterOrNULL", userAgent="character", timeout="numeric", verbose="logical", cache="logical", welcome="listOrNULL"))

#' @param .Object KorAPConnection object
#' @param KorAPUrl URL of the web user interface of the KorAP server instance you want to access.
#' @param apiVersion which version of KorAP's API you want to connect to.
#' @param apiUrl URL of the KorAP web service.
#' @param accessToken OAuth2 access token. For queries on corpus parts with restricted
#'   access (e.g. textual queries on IPR protected data), you need to authorize
#'   your application with an access token.
#'   You can obtain an access token using the [auth()] method.
#'
#'   More details are explained in the
#'   [authorization section](https://github.com/KorAP/RKorAPClient#authorization)
#'   of the RKorAPClient Readme on GitHub.
#'
#'   To use authorization based on an access token
#'   in subsequent queries, initialize your KorAP connection with:
#'
#'   ```
#'   kco <- new("KorAPConnection", accessToken="<access token>")
#'   ```
#'
#'   In order to make the API
#'   token persistent for the currently used `KorAPUrl` (you can have one
#'   token per KorAPUrl / KorAP server instance), use:
#'
#'   ```
#'   persistAccessToken(kco)
#'   ```
#'
#'   This will store it in your keyring using the
#'   [keyring::keyring-package]. Subsequent new("KorAPConnection") calls will
#'   then automatically retrieve the token from your keying. To stop using a
#'   persisted token, call `clearAccessToken(kco)`. Please note that for
#'   DeReKo, authorized queries will behave differently inside and outside the
#'   IDS, because of the special license situation. This concerns also cached
#'   results which do not take into account from where a request was issued. If
#'   you experience problems or unexpected results, please try `kco <-
#'   new("KorAPConnection", cache=FALSE)` or use
#'   [clearCache()] to clear the cache completely.
#'
#'   An alternative to using an access token is to use a browser-based oauth2 workflow
#'   to obtain an access token. This can be done with the [auth()] method.
#'
#' @param oauthClient     OAuth2 client object.
#' @param oauthScope      OAuth2 scope.
#' @param authorizationPossible logical that indicates if authorization is possible/necessary for the current KorAP instance. Automatically set during initialization.
#' @param userAgent user agent string.
#' @param timeout tineout in seconds for API requests (this does not influence server internal timeouts).
#' @param verbose logical that decides whether following operations will default to
#'   be verbose.
#' @param cache logical that decides if API calls are cached locally. You can clear
#'   the cache with [clearCache()].
#' @return [KorAPConnection()] object that can be used e.g. with
#'   [corpusQuery()]
#'
#' @import httr2
#' @examples
#' \dontrun{
#'
#' kcon <- new("KorAPConnection", verbose = TRUE)
#' kq <- corpusQuery(kcon, "Ameisenplage")
#' kq <- fetchAll(kq)
#' }
#'
#' \dontrun{
#'
#' kcon <- new("KorAPConnection", verbose = TRUE, accessToken="e739u6eOzkwADQPdVChxFg")
#' kq <- corpusQuery(kcon, "Ameisenplage", metadataOnly=FALSE)
#' kq <- fetchAll(kq)
#' kq@collectedMatches$snippet
#' }
#'
#' @rdname KorAPConnection-class
#' @export
setMethod("initialize", "KorAPConnection",
          function(.Object, KorAPUrl = "https://korap.ids-mannheim.de/", apiVersion = 'v1.0', apiUrl, accessToken = getAccessToken(KorAPUrl), oauthClient = NULL, oauthScope = "search match_info", userAgent = "R-KorAP-Client", timeout=240, verbose = FALSE, cache = TRUE) {
            .Object <- callNextMethod()
            m <- regexpr("https?://[^?]+", KorAPUrl, perl = TRUE)
            .Object@KorAPUrl <- regmatches(KorAPUrl, m)
            if (!endsWith(.Object@KorAPUrl, '/')) {
              .Object@KorAPUrl <- paste0(.Object@KorAPUrl, "/")
            }
            if (missing(apiUrl)) {
              .Object@apiUrl = paste0(.Object@KorAPUrl, 'api/', apiVersion, '/')
            } else {
              .Object@apiUrl = apiUrl
            }
            .Object@accessToken = accessToken
            .Object@oauthClient = oauthClient
            .Object@apiVersion = apiVersion
            .Object@userAgent = userAgent
            .Object@oauthScope = oauthScope
            .Object@timeout = timeout
            .Object@verbose = verbose
            .Object@cache = cache
            .Object@welcome = apiCall(.Object, .Object@apiUrl, json = FALSE, cache = FALSE, getHeaders = TRUE)
            if (!is.null(.Object@welcome)) {
              message(.Object@welcome[[2]])
            }
            .Object@indexRevision <- .Object@welcome[[1]][["x-index-revision"]]
            .Object
          })


accessTokenServiceName <- "RKorAPClientAccessToken"

setGeneric("persistAccessToken", function(kco, ...) standardGeneric("persistAccessToken") )

#' Persist current access token in keyring
#'
#' @param kco KorAPConnection object
#' @param accessToken access token to be persisted. If not supplied, the current access token of the KorAPConnection object will be used.
#' @return KorAPConnection object.
#'
#' @aliases persistAccessToken
#'
#' @import keyring
#' @export
#'
#' @examples
#' \dontrun{
#' kco <- new("KorAPConnection", accessToken="e739u6eOzkwADQPdVChxFg")
#' persistAccessToken(kco)
#'
#' kco <- new("KorAPConnection") %>% auth(app_id="<my application id>") %>% persistAccessToken()
#' }
#'
#' @seealso [clearAccessToken()], [auth()]
#'
setMethod("persistAccessToken", "KorAPConnection",  function(kco, accessToken = kco@accessToken) {
  if (! is.null(kco@oauthClient)) {
    warning("Short lived access tokens from a confidential application cannot be persisted.")
    return(kco)
  }
  if (is.null(accessToken))
    stop("It seems that you have not supplied any access token that could be persisted.", call. = FALSE)

  kco@accessToken <- accessToken
  key_set_with_value(accessTokenServiceName, kco@KorAPUrl, accessToken)
  return(kco)
})

setGeneric("clearAccessToken", function(kco) standardGeneric("clearAccessToken") )

#' Clear access token from keyring and KorAPConnection object
#'
#' @aliases clearAccessToken
#' @import keyring
#' @param kco KorAPConnection object
#' @return KorAPConnection object with access token set to `NULL`.
#' @export
#' @examples
#'
#' \dontrun{
#' kco <- new("KorAPConnection")
#' kco <- clearAccessToken(kco)
#' }
#'
#' @seealso [persistAccessToken()]
#'
setMethod("clearAccessToken", "KorAPConnection",  function(kco) {
  key_delete(accessTokenServiceName, kco@KorAPUrl)
  kco@accessToken <- NULL
  kco
})

generic_kor_app_id = "99FbPHH7RrN36hbndF7b6f"

kustvakt_redirekt_uri = "http://localhost:1410/"
kustvakt_auth_path = "settings/oauth/authorize"

oauthRefresh <- function(req, client, scope, kco) {
  httr2::req_oauth_auth_code(req, client,  scope = scope,
                             auth_url = paste0(kco@KorAPUrl, kustvakt_auth_path),
                             redirect_uri = kustvakt_redirekt_uri,
                             cache_key = kco@KorAPUrl)
}

setGeneric("auth", function(kco,  app_id = generic_kor_app_id, app_secret = NULL, scope = kco@oauthScope) standardGeneric("auth") )

#' Authorize RKorAPClient
#'
#' @aliases auth
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Authorize RKorAPClient to make KorAP queries and download results on behalf of the user.
#'
#' @param kco KorAPConnection object
#' @param app_id OAuth2 application id. Defaults to the generic KorAP client application id.
#' @param app_secret OAuth2 application secret. Used with confidential client applications. Defaults to `NULL`.
#' @param scope OAuth2 scope. Defaults to "search match_info".
#' @return KorAPConnection object with access token set in `@accessToken`.
#'
#' @importFrom httr2 oauth_client oauth_flow_auth_code
#' @examples
#' \dontrun{
#' kco <- new("KorAPConnection", verbose = TRUE) %>% auth()
#' df <- collocationAnalysis(kco, "focus([marmot/p=ADJA] {Ameisenplage})",
#'   leftContextSize=1, rightContextSize=0)
#' }
#'
#' @seealso [persistAccessToken()], [clearAccessToken()]
#'
#' @export
setMethod("auth", "KorAPConnection", function(kco, app_id = generic_kor_app_id, app_secret = NULL, scope = kco@oauthScope) {
  if ( kco@KorAPUrl != "https://korap.ids-mannheim.de/" & app_id == generic_kor_app_id) {
    warning(paste("You can use the default app_id only for the IDS Mannheim KorAP main instance for querying DeReKo. Please provide your own app_id for accesing", kco@KorAPUrl))
    return(kco)
  }
  if (is.null(kco@accessToken) || is.null(kco@welcome)) { # if access token is not set or invalid
    client <- if (! is.null(kco@oauthClient)) kco@oauthClient else
      httr2::oauth_client(
        id =  app_id,
        secret = app_secret,
        token_url = paste0(kco@apiUrl, "oauth2/token")
      )
    if (is.null(app_secret)) {
      kco@accessToken <- ( client |>
        httr2::oauth_flow_auth_code(
          scope = scope,
          auth_url = paste0(kco@KorAPUrl, kustvakt_auth_path),
          redirect_uri = kustvakt_redirekt_uri
        ))$access_token
      log_info(kco@verbose, "Client authorized. New access token set.")
    } else {
      kco@oauthClient <- client
      kco@oauthScope <- scope
      req <- request(kco@apiUrl) |>
        oauthRefresh(client, scope, kco) |>
        req_perform()
      log_info(kco@verbose, "Client authorized. Short lived access token will be refreshed automatically.")
    }
  } else {
    log_info(kco@verbose, "Access token already set.")
  }
  return(kco)
})



#' @import keyring
getAccessToken <- function(KorAPUrl) {
    keyList <- tryCatch(withCallingHandlers(key_list(service = accessTokenServiceName),
                                   warning = function(w) invokeRestart("muffleWarning"),
                                   error = function(e) return(NULL)),
                          error = function(e) { })
  if (KorAPUrl %in% keyList$username)
    key_get(accessTokenServiceName, KorAPUrl)
  else
    NULL
}


warnIfNoAccessToken <- function(kco) {
  if (is.null(kco@accessToken) & is.null(kco@oauthClient)) {
    warning(
      paste0(
        "In order to receive KWICSs also from corpora with restricted licenses, you may need to\n",
        "authorize your application with an access token or the auth() method.\n",
        "To generate an access token, login to KorAP and navigate to KorAP's OAuth settings <",
        kco@KorAPUrl,
        "settings/oauth#page-top>"
      )
    )
  }
}

KorAPCacheSubDir <- function() {
  paste0("RKorAPClient_",
         gsub(
           "^([0-9]+\\.[0-9]+).*",
           "\\1",
           packageVersion("RKorAPClient"),
           perl = TRUE
         ))
}

setGeneric("apiCall", function(kco, ...)  standardGeneric("apiCall") )

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @aliases apiCall
#' @rdname KorAPConnection-class
#' @param kco KorAPConnection object
#' @param url request url
#' @param json logical that determines if JSON result is expected
#' @param getHeaders logical that determines if headers and content should be returned (as a list)
#' @importFrom jsonlite fromJSON
#' @importFrom curl has_internet
#' @import httr2
#' @export
setMethod("apiCall", "KorAPConnection", function(kco, url, json = TRUE, getHeaders = FALSE, cache = kco@cache, timeout = kco@timeout) {
  result <- ""

  # Handle caching if enabled
  if (cache) {
    result <- R.cache::loadCache(dir = KorAPCacheSubDir(), key = list(url, kco@accessToken, kco@indexRevision))
    if (!is.null(result)) {
      if (!is.null(result$meta)) result$meta$cached <- "local"
      return(result)
    }
  }

  # Check for internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  # Create the request
  req <- httr2::request(url) |>
    httr2::req_user_agent(kco@userAgent) |>
    httr2::req_timeout(timeout)

  if (! is.null(kco@oauthClient)) {
    req <-  req |> oauthRefresh(kco@oauthClient, scope = kco@oauthScope, kco)
  } else if (!is.null(kco@accessToken)) {
    req <- req |> httr2::req_auth_bearer_token(kco@accessToken)
  }

  # Perform the request and handle errors
  resp <- tryCatch(
    req |> httr2::req_perform(),
    error = function(e) {
      message(if(kco@verbose) "\n" else "", "Request failed: ", paste(e$message, e$parent$message, sep = " "))
      e$resp
    }
  )
#
  if (is.null(resp)) return(invisible(NULL))

  # Check response status
  if (resp |> httr2::resp_status() != 200) {

    message("API request failed with status: ", resp |> httr2::resp_status())

    result <- tryCatch(
      resp |> httr2::resp_body_json(),
      error = function(e) {
        message("Failed to parse json with error details: ", e$message)
        return(NULL)
      }
    )
    # Handle errors in the response (if any)
    if (!is.null(result$errors)) {
      errors <- result$errors
      warning_msgs <- if (is.data.frame(errors)) {
        apply(errors, 1, function(warning) paste(warning[1], ": ", warning[2]))
      } else {
        lapply(errors, function(error) paste(error, collapse = " "))
      }
      message(paste(warning_msgs, collapse = "\n"))
    }

    return(invisible(NULL))
  }

  # Process JSON response or raw text based on `json` parameter
  if (json) {
    content_type <- resp |> httr2::resp_content_type()
    if (!content_type %in% c("application/json", "application/ld+json")) {
      message("API did not return JSON")
      return(invisible(NULL))
    }

    result <- tryCatch(
      resp |> httr2::resp_body_string() |> jsonlite::fromJSON(),
      error = function(e) {
        message("Failed to parse JSON: ", e$message)
        return(NULL)
      }
    )

    # Handle warnings in the response (if any)
    if (!is.null(result$warnings)) {
      warnings <- result$warnings
      warning_msgs <- if (is.data.frame(warnings)) {
        apply(warnings, 1, function(warning) paste(warning[1], ": ", warning[2]))
      } else {
        lapply(warnings, function(warning) paste(warning, collapse = " "))
      }
      message(paste(warning_msgs, collapse = "\n"))
    }
  } else {
    result <- resp |> httr2::resp_body_string()
  }

  # Save to cache if enabled
  if (cache) {
    R.cache::saveCache(result, key = list(url, kco@accessToken, kco@indexRevision), dir = KorAPCacheSubDir(), compress = TRUE)
  }

  # Return headers and content as a list if `getHeaders` is TRUE
  if (getHeaders) {
    list(headers = resp |> httr2::resp_headers(), content = result)
  } else {
    result
  }
})

setGeneric("clearCache", function(kco)  standardGeneric("clearCache") )

#' @aliases clearCache
#' @rdname KorAPConnection-class
#' @export
setMethod("clearCache", "KorAPConnection",  function(kco) {
  R.cache::clearCache(dir=KorAPCacheSubDir())
})

#' @rdname KorAPConnection-class
#' @param object KorAPConnection object
#' @export
setMethod("show", "KorAPConnection", function(object) {
  cat("<KorAPConnection>", "\n")
  cat("apiUrl: ", object@apiUrl, "\n")
})

##' Funtion KorAPConnection()
##'
##' Wrappper function for new("KorAPConnection")
##'
##' @rdname KorAPConnection-constructor
##' @name KorAPConnection-constructor
##' @export
## XKorAPConnection <- function(...) new("KorAPConnection", ...)
