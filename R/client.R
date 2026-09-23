# Talking to the platform. Everything else in this package builds on `sondavi_get()`.

#' Connect to a survey platform
#'
#' @param base_url The platform's address, e.g. "https://survey.example.org".
#'   The API lives under /api/v1 on your organisation's own domain.
#' @param token Your API token. Defaults to the SONDAVI_TOKEN environment
#'   variable, and that is where it belongs: a token written into an analysis
#'   script travels with the script into version control, onto shared drives and
#'   into supplementary material. Add it to ~/.Renviron instead.
#' @return A connection object for the other functions in this package.
#' @export
sondavi_connect <- function(base_url, token = Sys.getenv("SONDAVI_TOKEN")) {
  if (!nzchar(base_url)) stop("base_url is empty.", call. = FALSE)

  if (!nzchar(token)) {
    stop(
      "No token. Put it in ~/.Renviron as\n",
      "    SONDAVI_TOKEN=sdv_...\n",
      "and restart the R session. Create one under your account on the platform.",
      call. = FALSE
    )
  }

  structure(
    list(base_url = sub("/+$", "", base_url), token = token),
    class = "sondavi_connection"
  )
}

#' @export
print.sondavi_connection <- function(x, ...) {
  cat("<sondavi_connection>", x$base_url, "\n")
  cat("token:", paste0(substr(x$token, 1, 9), "..."), "\n")
  invisible(x)
}

# jsonlite is called by name rather than through httr2::resp_body_json(): httr2 only
# SUGGESTS jsonlite, so relying on it indirectly means the package works wherever
# jsonlite happens to be installed and fails everywhere else. Naming it here makes the
# dependency real - and makes `R CMD check` stop calling the Imports entry unused, which
# is what led to it being dropped in the first place.
parse_json <- function(resp) {
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

# A recognisable caller in the platform's audit trail - that is half the reason
# to ship a package at all. Works when the files are merely sourced, too.
sondavi_user_agent <- function() {
  v <- tryCatch(as.character(utils::packageVersion("sondavi")), error = function(e) "dev")
  paste0("sondavi/", v, " (R ", getRversion(), ")")
}

# One request, with the two things a hand-written call usually gets wrong:
# the server's own waiting time is honoured, and failures say what to do.
sondavi_get <- function(con, path, query = list(), max_tries = 4, body = NULL) {
  stopifnot(inherits(con, "sondavi_connection"))

  req <- httr2::request(paste0(con$base_url, "/api/v1/", path))
  req <- httr2::req_headers(req,
    Authorization = paste("Bearer", con$token),
    Accept = "application/json"
  )
  req <- httr2::req_user_agent(req, sondavi_user_agent())
  if (length(query)) req <- httr2::req_url_query(req, !!!query)

  # The platform limits per token and says how long to wait. Retrying without
  # reading Retry-After is how a script turns a small limit into a long outage.
  req <- httr2::req_retry(req, max_tries = max_tries, retry_on_failure = TRUE)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  if (!is.null(body)) req <- httr2::req_body_json(req, body)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)

  # 201 as well: recording a snapshot creates something (the platform records snapshots this way).
  if (status %in% c(200L, 201L)) {
    return(parse_json(resp))
  }

  message_of <- function(resp) {
    out <- try(parse_json(resp)$message, silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) "" else out
  }

  stop(switch(
    as.character(status),
    "401" = paste0(
      "The platform rejected the token (401). It may be unknown, expired or ",
      "revoked - the answer is deliberately the same for all three. Check ",
      "your account page."
    ),
    "403" = paste0("Refused (403): ", message_of(resp)),
    "404" = paste0(
      "Not found (404). Either the study does not exist, or this token was not ",
      "given access to it - the platform does not distinguish the two on purpose."
    ),
    "429" = paste0(
      "Rate limit reached (429) and still limited after ", max_tries,
      " attempts. Slow the loop down or fetch larger pages."
    ),
    paste0("The platform answered ", status, ". ", message_of(resp))
  ), call. = FALSE)
}

# Recording a snapshot is the one write this API has - it stores which responses a
# result was computed from, never their contents.
sondavi_post <- function(con, path, body) {
  sondavi_get(con, path, query = list(), body = body)
}
