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
#' @examples
#' \dontrun{
#' # The token comes from ~/.Renviron (SONDAVI_TOKEN=sdv_...), not from the
#' # script - a token written into a script travels with it into version
#' # control and into supplementary material.
#' con <- sondavi_connect("https://survey.example.org")
#' }
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
sondavi_get <- function(con, path, query = list(), max_tries = 4, body = NULL, dest = NULL) {
  sondavi_request(con, if (is.null(body)) "GET" else "POST", path, body, query, max_tries, dest)
}

# Recording a snapshot stores which responses a result was computed from; a simulation step
# hands the platform a response (SPECS §88). Neither changes a questionnaire.
sondavi_post <- function(con, path, body) {
  sondavi_request(con, "POST", path, body)
}

sondavi_request <- function(con, method, path, body = NULL, query = list(), max_tries = 4, dest = NULL) {
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
  # Except for a day's simulated turns being used up: that wait lasts until midnight,
  # so it is reported, not waited out.
  transient <- function(resp) {
    status <- httr2::resp_status(resp)
    if (status == 429L) return(!identical(reason_of(resp), "budget_exhausted"))
    status == 503L
  }
  retry <- list(req, max_tries = max_tries, is_transient = transient)
  # Retrying after a dropped connection came with httr2 1.0.1; older versions retry on
  # answers only, which is still right for everything above.
  if ("retry_on_failure" %in% names(formals(httr2::req_retry))) retry$retry_on_failure <- TRUE
  req <- do.call(httr2::req_retry, retry)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  if (!is.null(body)) req <- httr2::req_body_json(req, body, null = "null")
  req <- httr2::req_method(req, method)

  # A file (the uploaded files' ZIP) goes straight to disk instead of through memory.
  resp <- if (is.null(dest)) httr2::req_perform(req) else httr2::req_perform(req, path = dest)
  status <- httr2::resp_status(resp)

  # 201 as well: recording a snapshot or starting a simulation creates something.
  if (status %in% c(200L, 201L)) {
    return(if (is.null(dest)) parse_json(resp) else invisible(dest))
  }

  message_of <- function(resp) {
    out <- try(parse_json(resp)$message, silent = TRUE)
    if (inherits(out, "try-error") || is.null(out)) "" else out
  }
  reason <- reason_of(resp)

  if (status == 429L && identical(reason, "budget_exhausted")) {
    sondavi_abort(
      paste0(message_of(resp), " (Retry-After: ", httr2::resp_header(resp, "Retry-After"), " s)"),
      "sondavi_budget_exhausted",
      retry_after = suppressWarnings(as.integer(httr2::resp_header(resp, "Retry-After")))
    )
  }
  if (identical(reason, "model_failed") || identical(reason, "busy")) {
    sondavi_abort(message_of(resp), "sondavi_retryable", reason = reason)
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

reason_of <- function(resp) {
  out <- try(parse_json(resp)$reason, silent = TRUE)
  if (inherits(out, "try-error")) NULL else out
}

# An error a script can catch by its class, e.g.
# tryCatch(..., sondavi_budget_exhausted = function(e) e$retry_after)
sondavi_abort <- function(message, class, ...) {
  stop(structure(class = c(class, "sondavi_error", "error", "condition"),
                 list(message = message, call = NULL, ...)))
}
