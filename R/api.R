# The three things a researcher wants: what may I read, what do the variables
# mean, and give me the data.

#' The studies this token may read
#'
#' @param con A connection from [sondavi_connect()].
#' @return A data frame with one row per study.
#' @export
sondavi_surveys <- function(con) {
  body <- sondavi_get(con, "surveys")

  rows <- lapply(body$data, function(s) {
    data.frame(
      id = s$id,
      title = s$title %||% NA_character_,
      project = (s$project$name %||% NA_character_),
      status = s$status %||% NA_character_,
      privacy = s$privacy %||% NA_character_,
      response_count = s$response_count %||% NA_integer_,
      stringsAsFactors = FALSE
    )
  })

  if (!length(rows)) {
    return(data.frame(
      id = integer(), title = character(), project = character(),
      status = character(), privacy = character(), response_count = integer(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

#' What the variables of a study mean
#'
#' This is what a CSV cannot carry: the value labels and the intended type. It
#' is the same metadata the SPSS and R exports are built from.
#'
#' @param con A connection from [sondavi_connect()].
#' @param survey_id The study's id, from [sondavi_surveys()].
#' @return A data frame with one row per variable; `value_labels` is a list
#'   column of data frames with `value` and `text`.
#' @export
sondavi_codebook <- function(con, survey_id) {
  body <- sondavi_get(con, paste0("surveys/", survey_id, "/codebook"))
  vars <- body$variables

  if (!length(vars)) {
    return(data.frame(
      name = character(), label = character(), type = character(),
      surveyjs_type = character(), stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, lapply(vars, function(v) {
    data.frame(
      name = v$name %||% NA_character_,
      label = v$label %||% NA_character_,
      type = v$type %||% NA_character_,
      surveyjs_type = v$surveyjs_type %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))

  out$value_labels <- lapply(vars, function(v) {
    labs <- v$value_labels
    if (!length(labs)) {
      return(data.frame(value = character(), text = character(), stringsAsFactors = FALSE))
    }
    do.call(rbind, lapply(labs, function(l) {
      data.frame(value = as.character(l$value), text = as.character(l$text), stringsAsFactors = FALSE)
    }))
  })

  out
}

#' The responses of a study
#'
#' Walks the pages for you. With `labels = TRUE` (the default) the study's
#' codebook is applied, so categorical questions arrive as factors with their
#' real labels instead of bare codes.
#'
#' @param con A connection from [sondavi_connect()].
#' @param survey_id The study's id.
#' @param since Only responses recorded at or after this time. Anything
#'   [as.POSIXct()] understands, or an ISO-8601 string.
#' @param include_test Include responses collected through the test link.
#' @param labels Apply the codebook (factors and numeric types).
#' @param page_size Rows per request. Larger is fewer round trips.
#' @param max_rows Stop after this many rows. Useful while writing a script.
#' @return A data frame (a tibble if the package is installed). The dataset
#'   fingerprint is attached as the attribute `sondavi_fingerprint`; see
#'   [sondavi_fingerprint()].
#' @export
sondavi_responses <- function(con, survey_id, since = NULL, include_test = FALSE,
                          labels = TRUE, page_size = 500, max_rows = Inf) {
  query <- list(per_page = page_size)
  if (isTRUE(include_test)) query$include_test <- 1
  if (!is.null(since)) query$since <- format_since(since)

  pages <- list()
  rows_so_far <- 0
  cursor <- NULL
  first_meta <- NULL

  repeat {
    q <- query
    if (!is.null(cursor)) q$cursor <- cursor

    body <- sondavi_get(con, paste0("surveys/", survey_id, "/responses"), q)
    if (is.null(first_meta)) first_meta <- body$meta

    pages[[length(pages) + 1]] <- body$data
    rows_so_far <- rows_so_far + length(body$data)

    cursor <- body$meta$next_cursor
    if (is.null(cursor) || rows_so_far >= max_rows || !length(body$data)) break
  }

  flat <- unlist(pages, recursive = FALSE)
  if (is.finite(max_rows) && length(flat) > max_rows) flat <- flat[seq_len(max_rows)]

  out <- rows_to_frame(flat)
  if (labels && nrow(out)) out <- apply_codebook(out, sondavi_codebook(con, survey_id))

  # The fingerprint of the WHOLE walk, not of the first page: the rows the
  # caller actually holds are what a paper would have to name.
  attr(out, "sondavi_fingerprint") <- list(
    generated_at = first_meta$fingerprint$generated_at %||% NA_character_,
    rows = nrow(out),
    digest = first_meta$fingerprint$digest %||% NA_character_,
    survey_id = survey_id,
    abilities = unlist(first_meta$abilities %||% list())
  )

  if (requireNamespace("tibble", quietly = TRUE)) out <- tibble::as_tibble(out)
  out
}

#' The fingerprint of a fetched dataset
#'
#' A live query is not a dataset: run the same script tomorrow and it may return
#' different rows. Record this line next to the result, so a later run that
#' differs is recognisable as a different dataset rather than a silent correction.
#'
#' @param data The result of [sondavi_responses()].
#' @return A one-line character string, invisibly also printed.
#' @export
sondavi_fingerprint <- function(data) {
  fp <- attr(data, "sondavi_fingerprint")
  if (is.null(fp)) stop("This does not look like a result of sondavi_responses().", call. = FALSE)

  line <- sprintf(
    "survey %s, %d rows, fetched %s, digest %s",
    fp$survey_id, fp$rows, fp$generated_at, substr(fp$digest, 1, 16)
  )
  cat(line, "\n")
  invisible(line)
}

# -- helpers ------------

`%||%` <- function(a, b) if (is.null(a)) b else a

format_since <- function(since) {
  if (inherits(since, "POSIXt") || inherits(since, "Date")) {
    return(format(as.POSIXct(since), "%Y-%m-%dT%H:%M:%S%z"))
  }
  as.character(since)
}

# JSON objects to one rectangle. Responses of the same study can differ in which
# keys they carry (a skipped page leaves its questions out), so the union of keys
# decides the columns and what is missing becomes NA rather than shifting a row.
rows_to_frame <- function(rows) {
  if (!length(rows)) return(data.frame())

  keys <- unique(unlist(lapply(rows, names)))

  cols <- lapply(keys, function(k) {
    vals <- lapply(rows, function(r) {
      v <- r[[k]]
      if (is.null(v)) return(NA)
      # Nested answers (matrices, dynamic panels, jsPsych trials) stay as they
      # are, in a list column - flattening them here would invent a shape.
      if (is.list(v)) return(I(list(v)))
      if (length(v) > 1) return(I(list(v)))
      v
    })

    if (any(vapply(vals, function(v) inherits(v, "AsIs"), logical(1)))) {
      return(I(lapply(vals, function(v) if (inherits(v, "AsIs")) v[[1]] else v)))
    }
    unlist(vals, use.names = FALSE)
  })

  names(cols) <- keys
  as.data.frame(cols, stringsAsFactors = FALSE, optional = TRUE)
}

# The point of shipping a codebook: real factors, right types.
apply_codebook <- function(data, codebook) {
  for (i in seq_len(nrow(codebook))) {
    name <- codebook$name[i]
    if (!name %in% names(data)) next
    if (is.list(data[[name]])) next   # nested answers are left alone

    labs <- codebook$value_labels[[i]]
    type <- codebook$type[i]

    if (!is.null(labs) && nrow(labs)) {
      data[[name]] <- factor(as.character(data[[name]]), levels = labs$value, labels = labs$text)
    } else if (identical(type, "numeric")) {
      data[[name]] <- suppressWarnings(as.numeric(data[[name]]))
    }

    if (!is.na(codebook$label[i]) && nzchar(codebook$label[i])) {
      attr(data[[name]], "label") <- codebook$label[i]
    }
  }
  data
}

# -- Citable datasets ------------

#' Record the exact set of responses behind a result
#'
#' A live query is not a dataset: run the analysis again next month and it may
#' return more rows, or fewer. A snapshot names the set a published result was
#' computed from. It records which responses belonged to it - never a copy of
#' the answers, so retention and the right to erasure keep working.
#'
#' @param con A connection from [sondavi_connect()].
#' @param survey_id The study's id.
#' @param label Your own note, e.g. "Paper, figure 2".
#' @param since,include_test As in [sondavi_responses()].
#' @return A one-row data frame describing the snapshot.
#' @export
sondavi_snapshot <- function(con, survey_id, label = NULL, since = NULL, include_test = FALSE) {
  body <- list(include_test = isTRUE(include_test))
  if (!is.null(label)) body$label <- label
  if (!is.null(since)) body$since <- format_since(since)

  snapshot_frame(sondavi_post(con, paste0("surveys/", survey_id, "/snapshots"), body)$data)
}

#' The snapshots you have recorded
#' @param con A connection from [sondavi_connect()].
#' @export
sondavi_snapshots <- function(con) {
  rows <- sondavi_get(con, "snapshots")$data
  if (!length(rows)) return(snapshot_frame(NULL))
  do.call(rbind, lapply(rows, snapshot_frame))
}

#' The responses of a recorded set, as they are now
#'
#' The `complete` attribute is the point: `FALSE` means some of the responses are
#' gone - through retention or an erasure request - so the dataset behind a
#' published result no longer exists in full. That is information, not an error.
#'
#' @param con A connection from [sondavi_connect()].
#' @param snapshot_id The id from [sondavi_snapshot()].
#' @param labels Apply the codebook.
#' @export
sondavi_snapshot_responses <- function(con, snapshot_id, labels = TRUE) {
  body <- sondavi_get(con, paste0("snapshots/", snapshot_id, "/responses"))

  out <- rows_to_frame(body$data)
  meta <- body$meta$snapshot
  if (labels && nrow(out)) out <- apply_codebook(out, sondavi_codebook(con, meta$survey_id))

  attr(out, "sondavi_snapshot") <- snapshot_frame(meta)
  attr(out, "sondavi_complete") <- isTRUE(body$meta$matches_recorded)

  if (!isTRUE(body$meta$matches_recorded)) {
    warning(
      "This snapshot is no longer complete: ", meta$missing_rows, " of ",
      meta$recorded_rows, " responses have been deleted since it was recorded. ",
      "The dataset behind a result computed from it has changed.",
      call. = FALSE
    )
  }

  if (requireNamespace("tibble", quietly = TRUE)) out <- tibble::as_tibble(out)
  out
}

snapshot_frame <- function(s) {
  if (is.null(s)) {
    return(data.frame(id = character(), survey_id = integer(), label = character(),
                      recorded_at = character(), digest = character(),
                      recorded_rows = integer(), present_rows = integer(),
                      missing_rows = integer(), complete = logical(),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    id = s$id, survey_id = s$survey_id, label = s$label %||% NA_character_,
    recorded_at = s$recorded_at, digest = s$digest,
    recorded_rows = s$recorded_rows, present_rows = s$present_rows,
    missing_rows = s$missing_rows, complete = isTRUE(s$complete),
    stringsAsFactors = FALSE
  )
}
