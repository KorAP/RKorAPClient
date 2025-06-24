################################################################################
# Use setClassUnion to define the unholy NULL-data union as a virtual class.
################################################################################
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
# setOldClass("httr2_oauth_client")

#' Connect to KorAP Server
#'
#' `KorAPConnection()` creates a connection to a KorAP server for corpus queries.
#' This is your starting point for all corpus analysis tasks.
#'
#' Use `KorAPConnection()` to connect, then `corpusQuery()` to search, and
#' `fetchAll()` to retrieve results. For authorized access to restricted corpora,
#' use `auth()` or provide an `accessToken`.
#'
#' @section Basic Workflow:
#' ```r
#' # Connect to KorAP
#' kcon <- KorAPConnection()
#'
#' # Search for a term
#' query <- corpusQuery(kcon, "Ameisenplage")
#'
#' # Get all results
#' results <- fetchAll(query)
#' ```
#'
#' @section Authorization:
#' For access to restricted corpora, authorize your connection:
#' ```r
#' kcon <- KorAPConnection() |> auth()
#' ```
#'
#' @details
#' The KorAPConnection object contains various configuration slots for advanced users:
#' KorAPUrl (server URL), apiVersion, accessToken (OAuth2 token),
#' timeout (request timeout), verbose (logging), cache (local caching),
#' and other technical parameters. Most users can ignore these implementation details.
#'
#' @family initialization functions
#' @import R.cache
#' @import utils
#' @import methods
#' @include logging.R

#' @export
KorAPConnection <- setClass("KorAPConnection", slots = c(KorAPUrl = "character", apiVersion = "character", indexRevision = "characterOrNULL", apiUrl = "character", accessToken = "characterOrNULL", oauthClient = "ANY", oauthScope = "characterOrNULL", authorizationSupported = "logical", userAgent = "character", timeout = "numeric", verbose = "logical", cache = "logical", welcome = "listOrNULL"))

generic_kor_app_id <- "99FbPHH7RrN36hbndF7b6f"

kustvakt_redirect_uri <- "http://localhost:1410/"
kustvakt_auth_path <- "settings/oauth/authorize"


#' Initialize KorAPConnection object
#' @keywords internal
#' @param .Object KorAPConnection object
#' @param KorAPUrl URL of the web user interface of the KorAP server instance you want to access.
#'   Defaults to the environment variable `KORAP_URL` if set and to the IDS Mannheim KorAP main instance
#'   to query DeReKo, otherwise.
#' @param apiVersion which version of KorAP's API you want to connect to.
#' @param apiUrl URL of the KorAP web service.
#' @param accessToken OAuth2 access token. For queries on corpus parts with restricted
#'   access (e.g. textual queries on IPR protected data), you need to authorize
#'   your application with an access token.
#'   You can obtain an access token in the OAuth settings of your KorAP web interface.
#'
#'   More details are explained in the
#'   [authorization section](https://github.com/KorAP/RKorAPClient#authorization)
#'   of the RKorAPClient Readme on GitHub.
#'
#'   To use authorization based on an access token
#'   in subsequent queries, initialize your KorAP connection with:
#'
#'   ```
#'   kco <- KorAPConnection(accessToken="<access token>")
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
#'   [keyring::keyring-package]. Subsequent KorAPConnection() calls will
#'   then automatically retrieve the token from your keying. To stop using a
#'   persisted token, call `clearAccessToken(kco)`. Please note that for
#'   DeReKo, authorized queries will behave differently inside and outside the
#'   IDS, because of the special license situation. This concerns also cached
#'   results which do not take into account from where a request was issued. If
#'   you experience problems or unexpected results, please try `kco <-
#'   KorAPConnection(cache=FALSE)` or use
#'   [clearCache()] to clear the cache completely.
#'
#'   An alternative to using an access token is to use a browser-based oauth2 workflow
#'   to obtain an access token. This can be done with the [auth()] method.
#'
#' @param oauthClient     OAuth2 client object.
#' @param oauthScope      OAuth2 scope.
#' @param authorizationSupported logical that indicates if authorization is supported/necessary for the current KorAP instance. Automatically set during initialization.
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
#' kcon <- KorAPConnection(verbose = TRUE)
#' kq <- corpusQuery(kcon, "Ameisenplage")
#' kq <- fetchAll(kq)
#' }
#'
#' \dontrun{
#'
#' kcon <- KorAPConnection(verbose = TRUE, accessToken = "e739u6eOzkwADQPdVChxFg")
#' kq <- corpusQuery(kcon, "Ameisenplage", metadataOnly = FALSE)
#' kq <- fetchAll(kq)
#' kq@collectedMatches$snippet
#' }
#'

#' @export
setMethod("initialize", "KorAPConnection", function(.Object,
                                                    KorAPUrl = if (is.null(Sys.getenv("KORAP_URL")) |
                                                      Sys.getenv("KORAP_URL") == "") {
                                                      "https://korap.ids-mannheim.de/"
                                                    } else {
                                                      Sys.getenv("KORAP_URL")
                                                    },
                                                    apiVersion = "v1.0",
                                                    apiUrl,
                                                    accessToken = getAccessToken(KorAPUrl),
                                                    oauthClient = NULL,
                                                    oauthScope = "search match_info",
                                                    authorizationSupported = TRUE,
                                                    userAgent = "R-KorAP-Client",
                                                    timeout = 240,
                                                    verbose = FALSE,
                                                    cache = TRUE) {
  .Object <- callNextMethod()
  m <- regexpr("https?://[^?]+", KorAPUrl, perl = TRUE)
  .Object@KorAPUrl <- regmatches(KorAPUrl, m)
  if (!endsWith(.Object@KorAPUrl, "/")) {
    .Object@KorAPUrl <- paste0(.Object@KorAPUrl, "/")
  }
  if (missing(apiUrl)) {
    .Object@apiUrl <- paste0(.Object@KorAPUrl, "api/", apiVersion, "/")
  } else {
    .Object@apiUrl <- apiUrl
  }
  .Object@accessToken <- accessToken
  .Object@oauthClient <- oauthClient
  .Object@apiVersion <- apiVersion
  .Object@userAgent <- userAgent
  .Object@oauthScope <- oauthScope
  .Object@authorizationSupported <- authorizationSupported
  .Object@timeout <- timeout
  .Object@verbose <- verbose
  .Object@cache <- cache
  .Object@welcome <- apiCall(.Object, .Object@apiUrl, json = FALSE, cache = FALSE, getHeaders = TRUE)
  if (!is.null(.Object@welcome)) {
    message(.Object@welcome[[2]])
    resp <- httr2::request(.Object@KorAPUrl) |>
      httr2::req_url_path_append(kustvakt_auth_path) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()
    .Object@authorizationSupported <- (httr2::resp_status(resp) == 200)

    .Object@indexRevision <- .Object@welcome[[1]][["x-index-revision"]]
  } else {
    if (grepl(.Object@KorAPUrl, .Object@apiUrl)) {
      message("Could not connect to KorAP instance ", .Object@KorAPUrl)
    } else {
      message("Could not connect to KorAP API at ", .Object@apiUrl)
    }
  }
  .Object
})


accessTokenServiceName <- "RKorAPClientAccessToken"

setGeneric("persistAccessToken", function(kco, ...) standardGeneric("persistAccessToken"))

#' Persist current access token in keyring
#'
#' @family initialization functions
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
#' kco <- KorAPConnection(accessToken = "e739u6eOzkwADQPdVChxFg")
#' persistAccessToken(kco)
#'
#' kco <- KorAPConnection() %>%
#'   auth(app_id = "<my application id>") %>%
#'   persistAccessToken()
#' }
#'
#' @seealso [clearAccessToken()], [auth()]
#'
setMethod("persistAccessToken", "KorAPConnection", function(kco, accessToken = kco@accessToken) {
  if (!is.null(kco@oauthClient)) {
    warning("Short lived access tokens from a confidential application cannot be persisted.")
    return(kco)
  }
  if (is.null(accessToken)) {
    stop("It seems that you have not supplied any access token that could be persisted.", call. = FALSE)
  }

  kco@accessToken <- accessToken
  key_set_with_value(accessTokenServiceName, kco@KorAPUrl, accessToken)
  return(kco)
})

setGeneric("clearAccessToken", function(kco) standardGeneric("clearAccessToken"))

#' Clear access token from keyring and KorAPConnection object
#'
#' @family initialization functions
#' @aliases clearAccessToken
#' @import keyring
#' @param kco KorAPConnection object
#' @return KorAPConnection object with access token set to `NULL`.
#' @export
#' @examples
#' \dontrun{
#' kco <- KorAPConnection()
#' kco <- clearAccessToken(kco)
#' }
#'
#' @seealso [persistAccessToken()]
#'
setMethod("clearAccessToken", "KorAPConnection", function(kco) {
  key_delete(accessTokenServiceName, kco@KorAPUrl)
  kco@accessToken <- NULL
  kco
})


oauthRefresh <- function(req, client, scope, kco) {
  httr2::req_oauth_auth_code(req, client,
    scope = scope,
    auth_url = paste0(kco@KorAPUrl, kustvakt_auth_path),
    redirect_uri = kustvakt_redirect_uri,
    cache_key = kco@KorAPUrl
  )
}

setGeneric("auth", function(kco, app_id = generic_kor_app_id, app_secret = NULL, scope = kco@oauthScope) standardGeneric("auth"))

#' Authorize RKorAPClient
#'
#' @family initialization functions
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
#' kco <- KorAPConnection(verbose = TRUE) %>% auth()
#' df <- collocationAnalysis(kco, "focus([marmot/p=ADJA] {Ameisenplage})",
#'   leftContextSize = 1, rightContextSize = 0
#' )
#' }
#'
#' @seealso [persistAccessToken()], [clearAccessToken()]
#'
#' @export
setMethod("auth", "KorAPConnection", function(kco, app_id = generic_kor_app_id, app_secret = NULL, scope = kco@oauthScope) {
  if (kco@authorizationSupported == FALSE) {
    log_info(kco@verbose, "Authorization is not supported by this KorAP instance.")
    return(kco)
  }
  if (kco@KorAPUrl != "https://korap.ids-mannheim.de/" & app_id == generic_kor_app_id) {
    warning(paste("You can use the default app_id only for the IDS Mannheim KorAP main instance for querying DeReKo. Please provide your own app_id for accesing", kco@KorAPUrl))
    return(kco)
  }
  if (is.null(kco@accessToken) || is.null(kco@welcome)) { # if access token is not set or invalid
    client <- if (!is.null(kco@oauthClient)) {
      kco@oauthClient
    } else {
      httr2::oauth_client(
        id = app_id,
        secret = app_secret,
        token_url = paste0(kco@apiUrl, "oauth2/token")
      )
    }
    if (is.null(app_secret)) {
      kco@accessToken <- (client |>
        httr2::oauth_flow_auth_code(
          scope = scope,
          auth_url = paste0(kco@KorAPUrl, kustvakt_auth_path),
          redirect_uri = kustvakt_redirect_uri
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
  keyList <- tryCatch(
    withCallingHandlers(key_list(service = accessTokenServiceName),
      warning = function(w) invokeRestart("muffleWarning"),
      error = function(e) {
        return(NULL)
      }
    ),
    error = function(e) { }
  )
  if (KorAPUrl %in% keyList$username) {
    key_get(accessTokenServiceName, KorAPUrl)
  } else {
    NULL
  }
}


warnIfNotAuthorized <- function(kco) {
  if (kco@authorizationSupported & is.null(kco@accessToken) & is.null(kco@oauthClient)) {
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
  paste0(
    "RKorAPClient_",
    gsub(
      "^([0-9]+\\.[0-9]+).*",
      "\\1",
      packageVersion("RKorAPClient"),
      perl = TRUE
    )
  )
}

setGeneric("apiCall", function(kco, ...) standardGeneric("apiCall"))

## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c("."))

#' Internal API call method
#' @keywords internal
#' @aliases apiCall
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

  if (!is.null(kco@oauthClient)) {
    req <- req |> oauthRefresh(kco@oauthClient, scope = kco@oauthScope, kco)
  } else if (!is.null(kco@accessToken)) {
    req <- req |> httr2::req_auth_bearer_token(kco@accessToken)
  }

  resp <- tryCatch(req |> httr2::req_perform(),
    error = function(e) {
      if (is.null(e$resp)) {
        message(paste("Error: ", e$message, collapse = " "), if ("parent" %in% names(e)) paste0("\n", e$parent$message) else "")
        return(invisible(NULL))
      }
      return(e$resp)
    }
  )

  if (is.null(resp)) {
    return(invisible(NULL))
  }

  if (resp |> httr2::resp_status() != 200) {
    message("Error: Request failed with status ", resp |> httr2::resp_status(), ": ", resp |> httr2::resp_status_desc())
    if (resp |> httr2::resp_content_type() == "application/json") {
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
        message(paste("Warning: ", warning_msgs, collapse = "\n"))
      }
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
      message(paste0("\nWarning: ", paste(warning_msgs, collapse = " ")))
      if (cache & any(grepl("682", warning_msgs))) {
        cache <- FALSE
        log_info(kco@verbose, "Caching will be skipped because of warnings ")
      }
    }
  } else {
    result <- resp |> httr2::resp_body_string()
  }

  # Save to cache if enabled
  if (cache && resp |> httr2::resp_status() == 200) {
    R.cache::saveCache(result, key = list(url, kco@accessToken, kco@indexRevision), dir = KorAPCacheSubDir(), compress = TRUE)
  }

  # Return headers and content as a list if `getHeaders` is TRUE
  if (getHeaders) {
    list(headers = resp |> httr2::resp_headers(), content = result)
  } else {
    result
  }
})

setGeneric("clearCache", function(kco) standardGeneric("clearCache"))

#' Clear local cache
#'
#' Clears the local cache of API responses for the current RKorAPClient version.
#' Useful when you want to force fresh data retrieval or free up disk space.
#'
#' @family connection-initialization
#' @param kco KorAPConnection object
#' @return Invisible NULL (function called for side effects)
#' @examples
#' \dontrun{
#' kco <- KorAPConnection()
#' clearCache(kco)
#' }
#' 
#' @aliases clearCache
#' @export
setMethod("clearCache", "KorAPConnection", function(kco) {
  R.cache::clearCache(dir = KorAPCacheSubDir())
})

#' Display KorAPConnection object
#' @keywords internal
#' @param object KorAPConnection object
#' @export
setMethod("show", "KorAPConnection", function(object) {
  cat("<KorAPConnection>", "\n")
  cat("apiUrl: ", object@apiUrl, "\n")
})

##' Funtion KorAPConnection()
##'
##' Wrappper function for KorAPConnection()
##'
##' @rdname KorAPConnection-constructor
##' @name KorAPConnection-constructor
##' @export
## XKorAPConnection <- function(...) KorAPConnection(...)
