# The three things a researcher wants: what may I read, what do the variables
# mean, and give me the data.

#' The studies this token may read
#'
#' @param con A connection from [sondavi_connect()].
#' @return A data frame with one row per study.
#' @examples
#' \dontrun{
#' con <- sondavi_connect("https://survey.example.org")
#' sondavi_surveys(con)
#' }
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
#' @examples
#' \dontrun{
#' cb <- sondavi_codebook(con, 42)
#' cb[, c("name", "label", "type")]
#' cb$value_labels[[2]]          # the labels behind a categorical question
#' }
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
#' These are the same rows as the platform's JSON export, which means **partial
#' responses are included** when the study saves them: someone who stopped
#' halfway is a row whose `completed_at` is `NA`. `nrow()` is therefore not the
#' number of completed participations - use
#' `subset(d, !is.na(completed_at))` when that is what you mean.
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
#' @examples
#' \dontrun{
#' d <- sondavi_responses(con, 42)
#' table(d$mood)                             # a factor with its real labels
#' done <- subset(d, !is.na(completed_at))   # drop partial responses
#' mean(difftime(done$completed_at, done$started_at, units = "mins"))
#'
#' recent <- sondavi_responses(con, 42, since = Sys.Date() - 7)
#' }
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
  out <- parse_timestamps(out, first_meta$timezone)
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
#' @examples
#' \dontrun{
#' d <- sondavi_responses(con, 42)
#' sondavi_fingerprint(d)
#' #> survey 42, 318 rows, fetched 2026-09-23T09:32:29+00:00, digest 332f3d2a1fbfd880
#' }
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

# The platform's own timestamps, which every study carries. They arrive as
# wall-clock strings without a zone ("2026-09-23 10:45:29"), and left as text
# they are a quiet trap: `d$completed_at > Sys.time() - 7 * 86400` compares a
# character column against a date and returns an answer instead of an error.
# So does sorting by it. The API names its zone in the response; UTC is the
# fallback for an older instance that does not.
PLATFORM_TIMESTAMPS <- c("completed_at", "started_at")

parse_timestamps <- function(data, timezone = NULL) {
  tz <- if (is.null(timezone) || !nzchar(timezone)) "UTC" else timezone

  for (name in intersect(PLATFORM_TIMESTAMPS, names(data))) {
    value <- data[[name]]
    # An all-empty column arrives as logical NA; leave it, so the column keeps
    # saying "not recorded" rather than becoming a POSIXct full of NA.
    if (is.list(value) || !is.character(value)) next
    data[[name]] <- as.POSIXct(value, tz = tz, format = "%Y-%m-%d %H:%M:%S")
  }

  data
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
#' @examples
#' \dontrun{
#' snap <- sondavi_snapshot(con, 42, label = "Paper, figure 2")
#' snap$id        # the UUID that goes in the paper
#'
#' # Later, to reproduce. Warns if responses have been deleted since.
#' d <- sondavi_snapshot_responses(con, snap$id)
#' attr(d, "sondavi_complete")
#' }
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
#' The `sondavi_complete` attribute is the point: `FALSE` means some of the
#' responses are gone - through retention or an erasure request - so the dataset
#' behind a published result no longer exists in full. That is information, not
#' an error, and it comes with a warning naming how many are missing.
#'
#' `sondavi_read_as_recorded` says whether your token reads the same columns the
#' snapshot was recorded with. When it is `FALSE` the recorded fingerprint does
#' not describe what you received - the rows are narrower on purpose - so no
#' comparison is made and nothing is wrong.
#'
#' @param con A connection from [sondavi_connect()].
#' @param snapshot_id The id from [sondavi_snapshot()].
#' @param labels Apply the codebook.
#' @return A data frame of the recorded responses, carrying the attributes
#'   `sondavi_snapshot`, `sondavi_complete` and `sondavi_read_as_recorded`.
#' @export
sondavi_snapshot_responses <- function(con, snapshot_id, labels = TRUE) {
  body <- sondavi_get(con, paste0("snapshots/", snapshot_id, "/responses"))

  out <- rows_to_frame(body$data)
  out <- parse_timestamps(out, body$meta$timezone)
  meta <- body$meta$snapshot
  if (labels && nrow(out)) out <- apply_codebook(out, sondavi_codebook(con, meta$survey_id))

  attr(out, "sondavi_snapshot") <- snapshot_frame(meta)
  attr(out, "sondavi_complete") <- isTRUE(meta$complete)
  attr(out, "sondavi_read_as_recorded") <- isTRUE(body$meta$read_as_recorded)

  # Three different things, which one message used to conflate. Responses can be gone;
  # the answers themselves can have changed; or this token simply reads fewer columns
  # than the snapshot was recorded with, in which case the digests cannot be compared
  # at all and nothing is wrong.
  if (!isTRUE(meta$complete)) {
    warning(
      "This snapshot is no longer complete: ", meta$missing_rows, " of ",
      meta$recorded_rows, " responses have been deleted since it was recorded. ",
      "The dataset behind a result computed from it has changed.",
      call. = FALSE
    )
  } else if (isTRUE(body$meta$read_as_recorded) && !isTRUE(body$meta$matches_recorded)) {
    warning(
      "Every recorded response is still there, but their contents no longer match what ",
      "was recorded. A result computed from this snapshot may no longer reproduce.",
      call. = FALSE
    )
  } else if (!isTRUE(body$meta$read_as_recorded)) {
    message(
      "Your token reads fewer columns than this snapshot was recorded with, so the ",
      "recorded fingerprint does not apply to what you received. All ",
      meta$recorded_rows, " responses are still there."
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

#' Spread nested answers into one column each
#'
#' Matrices, dynamic panels and jsPsych trials arrive as list columns, because
#' flattening them on arrival would invent a shape the platform did not give.
#' When you do want the flat form, this produces exactly the columns the
#' platform's own export writes, so a script built on the export file and one
#' built on the API agree on the variable names.
#'
#' A matrix cell becomes `question.row.column`, an entry of a dynamic panel
#' `question.0.field` — counting from zero, as the export does, not from one as
#' R would. Anything still not rectangular (a multiple-choice cell) stays a list
#' column rather than being pasted into a string.
#'
#' @param data The result of [sondavi_responses()] or
#'   [sondavi_snapshot_responses()].
#' @param columns Which list columns to spread. Default: all of them.
#' @return The data frame with each nested column replaced, in place, by its
#'   flat columns. Attributes such as the fingerprint are kept.
#' @examples
#' \dontrun{
#' d <- sondavi_responses(con, 42)
#' names(d)                       # ... "ratings" (a list column)
#' flat <- sondavi_unnest(d)
#' names(flat)                    # ... "ratings.speed.score" "ratings.clarity.score"
#'
#' sondavi_unnest(d, columns = "ratings")   # leave the others nested
#' }
#' @export
sondavi_unnest <- function(data, columns = NULL) {
  nested <- names(data)[vapply(data, function(x) is.list(x) && !is.data.frame(x), logical(1))]
  targets <- if (is.null(columns)) nested else intersect(columns, nested)

  if (!length(targets)) return(data)

  kept <- attributes(data)[setdiff(names(attributes(data)), c("names", "row.names", "class"))]
  out <- data

  for (column in targets) {
    # Reusing rows_to_frame is what keeps this consistent with the top level: the
    # union of keys decides the columns, a missing one becomes NA rather than
    # shifting a row, and a leaf that is itself several values stays a list.
    spread <- rows_to_frame(lapply(out[[column]], function(v) flatten_answer(v, column)))

    at <- match(column, names(out))
    before <- if (at > 1) out[, seq_len(at - 1), drop = FALSE] else out[, 0, drop = FALSE]
    after <- if (at < ncol(out)) out[, seq(at + 1, ncol(out)), drop = FALSE] else out[, 0, drop = FALSE]

    out <- if (ncol(spread)) cbind(before, spread, after) else cbind(before, after)
  }

  for (name in names(kept)) attr(out, name) <- kept[[name]]
  if (requireNamespace("tibble", quietly = TRUE)) out <- tibble::as_tibble(out)
  out
}

# One stored answer to a named, flat list. Objects contribute their names,
# arrays their zero-based position - which is the export's convention and NOT
# R's, so the +1/-1 lives here, once, rather than in every analysis script.
flatten_answer <- function(value, prefix) {
  if (is.null(value) || (!is.list(value) && length(value) == 1 && is.na(value))) return(list())
  if (!is.list(value)) {
    leaf <- list(value)
    names(leaf) <- prefix
    return(leaf)
  }
  if (!length(value)) return(list())

  keys <- names(value)
  parts <- lapply(seq_along(value), function(i) {
    key <- if (is.null(keys) || !nzchar(keys[i])) as.character(i - 1L) else keys[i]
    flatten_answer(value[[i]], paste0(prefix, ".", key))
  })

  unlist(parts, recursive = FALSE)
}

#' Join the waves of a study series into one wide table
#'
#' Fetches several studies and matches their responses by the same person. Each
#' wave's columns are suffixed with its name, so `mood` becomes `mood_wave1` and
#' `mood_wave2` instead of colliding.
#'
#' **Everyone is kept, including those who did not take part in every wave** -
#' their later columns are `NA`. That is a full outer join rather than an inner
#' one on purpose: attrition is usually what a longitudinal design is about, and
#' quietly dropping the people who stopped answering would remove exactly the
#' cases you want to describe. `is.na(mood_wave2)` is the attrition indicator.
#'
#' The match needs an identifier, so the token must carry the respondent
#' ability and the studies must not be anonymous. Without it there is nothing to
#' join on and the function says so rather than guessing.
#'
#' @param con A connection from [sondavi_connect()].
#' @param survey_ids The studies, in wave order.
#' @param names One name per study. Default `wave1`, `wave2`, ...
#' @param by The column that identifies a person across waves.
#' @param ... Passed on to [sondavi_responses()], e.g. `include_test`.
#' @return One data frame, one row per person seen in any wave.
#' @examples
#' \dontrun{
#' d <- sondavi_waves(con, c(42, 43, 44))
#' table(complete = !is.na(d$mood_wave3))     # who made it to the last wave
#'
#' sondavi_waves(con, c(42, 43), names = c("baseline", "followup"))
#' }
#' @export
sondavi_waves <- function(con, survey_ids, names = NULL, by = "respondent_id", ...) {
  if (length(survey_ids) < 2) stop("Give at least two studies to join.", call. = FALSE)

  labels <- if (is.null(names)) paste0("wave", seq_along(survey_ids)) else names
  if (length(labels) != length(survey_ids)) {
    stop("`names` must have one entry per study.", call. = FALSE)
  }

  waves <- lapply(seq_along(survey_ids), function(i) {
    d <- sondavi_responses(con, survey_ids[[i]], ...)

    if (!by %in% base::names(d)) {
      stop(
        "Study ", survey_ids[[i]], " returned no `", by, "`, so its responses cannot be ",
        "matched to another wave. Either the token was created without the respondent ",
        "identifier, or the study is anonymous - in which case the answers are not ",
        "linkable by design.",
        call. = FALSE
      )
    }

    others <- setdiff(base::names(d), by)
    base::names(d)[match(others, base::names(d))] <- paste0(others, "_", labels[[i]])
    d
  })

  Reduce(function(a, b) merge(a, b, by = by, all = TRUE), waves)
}
