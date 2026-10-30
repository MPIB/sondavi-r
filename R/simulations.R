# -- Simulations ------------
#
# A script plays a study's AI interviews: the platform runs the interviewer exactly as it
# runs live, the script plays the respondent. Each simulation runs with a parameter set -
# the study's own settings, changed where the script says - and keeps whatever data the
# script attaches to it.

#' The parts of a study that can be simulated
#'
#' Today: the interview over the whole survey (`survey-interview`) and every
#' interview question (`question:<name>`). Each comes with its parameters - where
#' `default` is the value the study has now - and the inputs it takes.
#'
#' Needs a token with the permission "Run simulations".
#'
#' @param con A connection from [sondavi_connect()].
#' @param survey_id The study's id.
#' @return A data frame, one row per component, with the list columns
#'   `parameters` and `inputs` (data frames).
#' @examples
#' \dontrun{
#' comps <- sondavi_components(con, 42)
#' comps$id
#' comps$parameters[[1]][, c("name", "type")]
#' }
#' @export
sondavi_components <- function(con, survey_id) {
  rows <- sondavi_get(con, paste0("surveys/", survey_id, "/components"))$data
  out <- data.frame(
    id = vapply(rows, function(c) c$id, ""),
    kind = vapply(rows, function(c) c$kind, ""),
    title = vapply(rows, function(c) c$title, ""),
    stringsAsFactors = FALSE
  )
  out$parameters <- I(lapply(rows, function(c) parameter_frame(c$parameters)))
  out$inputs <- I(lapply(rows, function(c) parameter_frame(c$inputs)))
  out
}

#' The parameters of one component
#'
#' @inheritParams sondavi_components
#' @param component A component id, e.g. "question:q7".
#' @return A data frame: `name`, `type`, `description`, and the list columns
#'   `default` (the study's current value), `choices`; `min` and `max` where set.
#' @export
sondavi_parameters <- function(con, survey_id, component) {
  comps <- sondavi_components(con, survey_id)
  hit <- which(comps$id == component)
  if (!length(hit)) {
    stop("This study has no component \"", component, "\". It has: ",
         paste(comps$id, collapse = ", "), call. = FALSE)
  }
  comps$parameters[[hit]]
}

parameter_frame <- function(items) {
  if (!length(items)) {
    return(data.frame(name = character(), type = character(), description = character(),
                      stringsAsFactors = FALSE))
  }
  num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  out <- data.frame(
    name = vapply(items, function(p) p$name, ""),
    type = vapply(items, function(p) p$type, ""),
    description = vapply(items, function(p) if (is.null(p$description)) "" else p$description, ""),
    min = vapply(items, function(p) num(p$min), 0),
    max = vapply(items, function(p) num(p$max), 0),
    stringsAsFactors = FALSE
  )
  out$default <- I(lapply(items, function(p) p$default))
  out$choices <- I(lapply(items, function(p) unlist(p$choices)))
  out
}

#' Start a simulation
#'
#' Runs the component's opening. The returned simulation's `stimulus` is what a
#' respondent would see now; answer it with [sondavi_respond()], or let
#' [sondavi_simulate()] play the whole simulation.
#'
#' Parameters you leave out keep the study's value. A parameter the component
#' does not take, or a value of the wrong type or out of range, is refused
#' rather than replaced - an optimiser should measure what it thinks it measures.
#'
#' Starting uses one of your organisation's simulated turns for the day.
#'
#' @inheritParams sondavi_parameters
#' @param parameters A named list of parameter values, e.g.
#'   `list(systemPrompt = "Ask like a journalist.", temperature = 0.3)`.
#' @param inputs A named list: `locale` (e.g. "de"), `context` (earlier
#'   answers, interview question), `group` (experimental arm, whole-survey
#'   interview).
#' @param label,run Your own name for it, and a name grouping several (e.g. one
#'   optimisation).
#' @return A `sondavi_simulation`.
#' @export
sondavi_simulation_start <- function(con, survey_id, component, parameters = list(),
                                     inputs = list(), label = NULL, run = NULL) {
  body <- list(component = component)
  # Named lists, so an empty one still travels as a JSON object.
  if (length(parameters)) body$parameters <- as_object(parameters)
  if (length(inputs)) body$inputs <- as_object(inputs)
  if (!is.null(label)) body$label <- label
  if (!is.null(run)) body$run <- run
  as_simulation(sondavi_post(con, paste0("surveys/", survey_id, "/simulations"), body)$data, con)
}

#' Respond to a simulation's current stimulus
#'
#' Give exactly one of `text`, `value`, `skip` or `choice`, as the stimulus's
#' `expects` says: `text` an answer; with `"choice"` (a question answered with
#' buttons, which takes nothing typed) a `value` it offers - one of
#' `question$choices`, `question$special_items` or `question$rate_values` -,
#' with the text of "Other" in `other` and a `comment` where the question has a
#' comment area, or `skip = TRUE` where `question$can_skip`; `choice` the index
#' of a summary (from 0), or `NA` to skip the summaries. With a summary, `text`
#' asks for new ones.
#'
#' A model call that fails records nothing on the platform, so the same
#' response is sent again, up to `retries` times.
#'
#' @param sim A simulation from [sondavi_simulation_start()].
#' @param text,value,choice The response.
#' @param skip `TRUE` to leave a question with buttons out, where it may be.
#' @param other,comment With a `value`: the text of "Other", a comment.
#' @param retries How often to send it again after a failed model call.
#' @return The simulation, updated.
#' @export
sondavi_respond <- function(sim, text = NULL, value = NULL, choice = NULL, skip = FALSE,
                            other = NULL, comment = NULL, retries = 2) {
  stopifnot(inherits(sim, "sondavi_simulation"))
  given <- c(!is.null(text), !is.null(value), !is.null(choice), isTRUE(skip))
  if (sum(given) != 1) stop("Give exactly one of text, value, skip or choice.", call. = FALSE)
  if ((!is.null(other) || !is.null(comment)) && is.null(value)) {
    stop("`other` and `comment` go with a value.", call. = FALSE)
  }

  # `choice = NA` is "skip the summaries"; it travels as JSON null.
  body <- if (given[1]) list(text = text) else if (given[2]) c(list(value = value), list(other = other, comment = comment)[!vapply(list(other, comment), is.null, TRUE)]) else
    if (given[4]) list(skip = TRUE) else list(choice = if (is.na(choice)) NA else as.integer(choice))

  for (attempt in seq_len(retries + 1)) {
    out <- tryCatch(
      sondavi_post(sim$con, paste0("simulations/", sim$id, "/responses"), body),
      sondavi_retryable = function(e) if (attempt <= retries) e else stop(e)
    )
    if (!inherits(out, "condition")) return(as_simulation(out$data, sim$con))
    Sys.sleep(attempt)
  }
}

#' Play a simulation to its end
#'
#' `responder` is a function of the stimulus (a list with `messages`, `expects`,
#' and `question` or `options`) that returns the response: a string (an answer,
#' or with a summary a wish for new ones), or a list of [sondavi_respond()]'s
#' arguments - `list(value = "other", other = "...")`, `list(skip = TRUE)` ... With
#' a summary, a number is taken as the choice; where a question is answered with
#' buttons, anything but a string as its value.
#'
#' @inheritParams sondavi_simulation_start
#' @param responder A function of one stimulus.
#' @param max_steps A safety limit.
#' @return The ended `sondavi_simulation`.
#' @examples
#' \dontrun{
#' persona <- function(stimulus) {
#'   if (identical(stimulus$expects, "summary")) return(0)
#'   "I teach maths, mostly to teenagers."
#' }
#' sim <- sondavi_simulate(con, 42, "question:q7", persona,
#'                         parameters = list(systemPrompt = "Ask like a journalist."))
#' sondavi_transcript(sim)
#' sim <- sondavi_attach(sim, score = 0.8)
#' }
#' @export
sondavi_simulate <- function(con, survey_id, component, responder, parameters = list(),
                             inputs = list(), label = NULL, run = NULL, max_steps = 100) {
  sim <- sondavi_simulation_start(con, survey_id, component, parameters, inputs, label, run)

  for (i in seq_len(max_steps)) {
    if (!identical(sim$status, "active")) return(sim)
    answer <- responder(sim$stimulus)
    expects <- sim$stimulus$expects

    sim <- if (is.list(answer)) {
      do.call(sondavi_respond, c(list(sim), answer))
    } else if (identical(expects, "summary") && (is.numeric(answer) || (length(answer) == 1 && is.na(answer)))) {
      sondavi_respond(sim, choice = answer)
    } else if (identical(expects, "choice") && !is.character(answer)) {
      sondavi_respond(sim, value = answer)
    } else {
      sondavi_respond(sim, text = as.character(answer))
    }
  }

  stop("Simulation ", sim$id, " still running after ", max_steps, " steps.", call. = FALSE)
}

#' Keep your own data with a simulation
#'
#' Keys are added or replaced; a key set to `NULL` in `data` is removed. Works
#' also after the simulation has ended. At most 64 KB in all.
#'
#' @param sim A simulation.
#' @param ... Named values, e.g. `score = 0.8, feedback = "too long"`.
#' @param data A named list instead of, or in addition to, `...`.
#' @return The simulation, updated.
#' @export
sondavi_attach <- function(sim, ..., data = list()) {
  stopifnot(inherits(sim, "sondavi_simulation"))
  values <- c(data, list(...))
  if (!length(values) || is.null(names(values)) || any(names(values) == "")) {
    stop("Give named values to attach.", call. = FALSE)
  }
  # A NULL value means "remove" and travels as JSON null.
  body <- list(attachments = as_object(values))
  as_simulation(sondavi_request(sim$con, "PATCH", paste0("simulations/", sim$id), body)$data, sim$con)
}

#' One simulation, with its steps and result
#' @param con A connection.
#' @param simulation_id The simulation's id.
#' @export
sondavi_simulation <- function(con, simulation_id) {
  as_simulation(sondavi_get(con, paste0("simulations/", simulation_id))$data, con)
}

#' The simulations of the studies your token covers
#'
#' Newest first, as a data frame. Their steps are not included; fetch one with
#' [sondavi_simulation()].
#'
#' @param con A connection.
#' @param survey,run,component,status Optional filters.
#' @return A data frame with the list columns `parameters` and `attachments`.
#' @export
sondavi_simulations <- function(con, survey = NULL, run = NULL, component = NULL, status = NULL) {
  query <- list(survey = survey, run = run, component = component, status = status, per_page = 500)
  query <- query[!vapply(query, is.null, TRUE)]
  rows <- list()
  repeat {
    body <- sondavi_get(con, "simulations", query)
    rows <- c(rows, body$data)
    if (is.null(body$meta$next_cursor)) break
    query$cursor <- body$meta$next_cursor
  }
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  out <- data.frame(
    id = vapply(rows, function(s) s$id, ""),
    survey_id = vapply(rows, function(s) as.integer(s$survey_id), 0L),
    component = vapply(rows, function(s) s$component, ""),
    status = vapply(rows, function(s) s$status, ""),
    label = vapply(rows, function(s) chr(s$label), ""),
    run = vapply(rows, function(s) chr(s$run), ""),
    turns = vapply(rows, function(s) as.integer(s$usage$turns), 0L),
    created_at = vapply(rows, function(s) chr(s$created_at), ""),
    stringsAsFactors = FALSE
  )
  out$parameters <- I(lapply(rows, function(s) s$parameters))
  out$attachments <- I(lapply(rows, function(s) s$attachments))
  out
}

#' Delete a simulation you started
#' @param sim A simulation.
#' @export
sondavi_simulation_delete <- function(sim) {
  stopifnot(inherits(sim, "sondavi_simulation"))
  sondavi_request(sim$con, "DELETE", paste0("simulations/", sim$id))
  invisible(NULL)
}

#' The conversation of a simulation
#' @param sim A simulation.
#' @return A data frame with `role` ("interviewer" or "respondent") and `text`.
#' @export
sondavi_transcript <- function(sim) {
  stopifnot(inherits(sim, "sondavi_simulation"))
  if (is.null(sim$result)) sim <- sondavi_simulation(sim$con, sim$id)
  turns <- sim$result$transcript
  data.frame(
    role = vapply(turns, function(t) t$role, ""),
    text = vapply(turns, function(t) t$text, ""),
    stringsAsFactors = FALSE
  )
}

#' @export
print.sondavi_simulation <- function(x, ...) {
  cat("<sondavi_simulation>", x$id, "\n")
  cat(x$component, "-", x$status, "-", x$usage$turns, "turn(s)\n")
  if (identical(x$status, "active") && length(x$stimulus$messages)) {
    cat("waiting for", x$stimulus$expects, "after:", utils::tail(unlist(x$stimulus$messages), 1), "\n")
  }
  invisible(x)
}

as_simulation <- function(body, con) {
  structure(c(body, list(con = con)), class = "sondavi_simulation")
}

# A named list that stays a JSON object even when empty.
as_object <- function(x) {
  if (!length(x)) return(structure(list(), names = character()))
  x
}
