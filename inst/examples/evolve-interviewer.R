# An evolutionary search over an interviewer's settings, through simulations on the platform.
#
#   Sys.setenv(SONDAVI_TOKEN = "sdv_...")      # better: in ~/.Renviron; a token with "Run simulations"
#   source(system.file("examples", "evolve-interviewer.R", package = "sondavi"))
#   best <- evolve_interviewer("https://survey.example.org", 42, "question:q7")
#
# A genetic algorithm. A candidate ("genome") is
#   - which of a handful of interviewing rules are added to the study's own instructions
#     (a bit per rule),
#   - the temperature, and
#   - the number of turns (interview question) or follow-ups (whole-survey interview).
# Each candidate is played against a few personas - one simulation each - and a judge scores
# the conversation. The judge's score and reasons are attached to each simulation, so every
# candidate can be looked at, and the best one applied, under the survey's Simulations tab.
#
# The personas and the judge run on a language model of YOUR OWN (any OpenAI-compatible
# endpoint: EXAMPLE_LLM_URL, EXAMPLE_LLM_KEY, EXAMPLE_LLM_MODEL). Without EXAMPLE_LLM_URL a
# scripted persona and a rule-based judge are used, so the mechanics can be watched first.
# The platform's interviewer runs either way and uses your organisation's simulated turns:
# about generations x population x personas x (turns + 1).

if (!exists("sondavi_simulate", mode = "function")) library(sondavi)   # or the sources, while working on them

RULES <- c(
  one_question = "Ask exactly one question per message.",
  short        = "Keep every message under two sentences.",
  open         = "Ask open questions; never suggest an answer.",
  neutral      = "Do not praise or judge answers; acknowledge briefly and neutrally.",
  follow_up    = "Follow up on what the respondent just said before moving on."
)

PERSONAS <- list(
  list(name = "talkative teacher", description = "A 45-year-old teacher who likes to talk and drifts into anecdotes.",
       facts = c("I teach maths to teenagers.", "Mornings are the hardest.", "I go running after school.")),
  list(name = "terse nurse", description = "A 31-year-old night-shift nurse, tired, answers in a few words.",
       facts = c("Night shift.", "Busy ward.", "Sleeping during the day is hard.")),
  list(name = "careful retiree", description = "A 72-year-old retired engineer who answers precisely.",
       facts = c("Retired since 2019.", "I garden every morning.", "Library on Thursdays."))
)

# -- a language model of your own (optional) --

own_model <- function() {
  url <- Sys.getenv("EXAMPLE_LLM_URL")
  if (!nzchar(url)) return(NULL)
  model <- Sys.getenv("EXAMPLE_LLM_MODEL", "gpt-4o-mini")
  key <- Sys.getenv("EXAMPLE_LLM_KEY")
  function(prompt, temperature = 0.7) {
    req <- httr2::request(paste0(sub("/+$", "", url), "/chat/completions"))
    if (nzchar(key)) req <- httr2::req_auth_bearer_token(req, key)
    req <- httr2::req_body_json(req, list(model = model, temperature = temperature,
                                          messages = list(list(role = "user", content = prompt))))
    httr2::resp_body_json(httr2::req_perform(req))$choices[[1]]$message$content
  }
}

# An answer to a question with buttons: one of what it offers, in turn.
pick <- function(q, n) {
  if (identical(q$type, "boolean")) return(list(value = n %% 2 == 0))
  offered <- Filter(function(c) !isTRUE(c$disabled), c(q$choices, q$rate_values))
  if (length(offered)) {
    value <- offered[[(n %% length(offered)) + 1]]$value
    return(list(value = if (identical(q$type, "checkbox")) list(value) else value))
  }
  if (isTRUE(q$can_skip)) return(list(skip = TRUE))
  list(value = "other", other = "something else")
}

responder <- function(persona, llm) {
  said <- character()
  function(stimulus) {
    if (identical(stimulus$expects, "summary")) return(0)
    # A question answered with buttons takes one of them, nothing typed.
    if (identical(stimulus$expects, "choice")) return(pick(stimulus$question, length(said)))
    reply <- if (is.null(llm)) {
      persona$facts[(length(said) %% length(persona$facts)) + 1]
    } else {
      llm(paste0("You are taking part in a research interview. You are: ", persona$description,
                 "\nFacts about you: ", paste(persona$facts, collapse = " "),
                 "\nThe interviewer says: ", paste(unlist(stimulus$messages), collapse = " "),
                 "\nAnswer as this person would, in one to three sentences."), temperature = 0.9)
    }
    said <<- c(said, reply)
    reply
  }
}

judge <- function(transcript, persona, llm) {
  if (!is.null(llm)) {
    text <- paste(ifelse(transcript$role == "interviewer", "Interviewer", "Respondent"), transcript$text, sep = ": ", collapse = "\n")
    reply <- llm(paste0(
      "Score the INTERVIEWER from 0 to 10: one short, open, non-leading question at a time; follows up on ",
      "what was said; neutral.\nRespondent: ", persona$description, "\n\n", text,
      '\n\nAnswer with JSON only: {"score": <0-10>, "feedback": "<what to do differently>"}'), temperature = 0)
    verdict <- tryCatch(jsonlite::fromJSON(regmatches(reply, regexpr("\\{.*\\}", reply))), error = function(e) list(score = 0, feedback = reply))
    return(list(score = min(1, max(0, as.numeric(verdict$score) / 10)), feedback = as.character(verdict$feedback)))
  }
  # Rules that can be checked without a model.
  turns <- utils::head(transcript$text[transcript$role == "interviewer"], -1)
  if (!length(turns)) turns <- ""
  problems <- c(
    if (any(lengths(regmatches(turns, gregexpr("\\?", turns))) > 1)) "asked several questions in one message",
    if (any(nchar(turns) > 220)) "messages longer than 220 characters",
    if (any(!grepl("\\?", turns))) "a message without a question"
  )
  list(score = max(0, 1 - 0.25 * length(problems)),
       feedback = if (length(problems)) paste(problems, collapse = "; ") else "no rule broken")
}

# -- the algorithm --

evolve_interviewer <- function(base_url, survey_id, component, generations = 3, population = 4,
                               personas = 2, mutation = 0.2, run = "evolve-r", seed = 1) {
  set.seed(seed)
  con <- sondavi_connect(base_url)
  llm <- own_model()
  params <- sondavi_parameters(con, survey_id, component)
  kind <- if (startsWith(component, "question:")) "question" else "survey"
  genes <- if (kind == "question") c(text = "systemPrompt", count = "maxTurns") else c(text = "system_prompt", count = "max_follow_ups")
  base_text <- params$default[[which(params$name == genes[["text"]])]]
  count_max <- min(4, params$max[params$name == genes[["count"]]])
  count_min <- max(1, params$min[params$name == genes[["count"]]])

  settings_of <- function(g) {
    out <- list(paste(c(base_text, RULES[as.logical(g$rules)]), collapse = "\n"), g$temperature, as.integer(g$count))
    names(out) <- c(genes[["text"]], "temperature", genes[["count"]])
    out
  }
  random_genome <- function() list(rules = rbinom(length(RULES), 1, 0.3), temperature = round(runif(1, 0, 1.2), 2),
                                   count = sample(count_min:count_max, 1))
  fitness <- function(g, generation) {
    scores <- vapply(sample(PERSONAS, personas), function(p) {
      sim <- sondavi_simulate(con, survey_id, component, responder(p, llm), parameters = settings_of(g),
                              label = paste0("g", generation, " ", p$name), run = run)
      verdict <- judge(sondavi_transcript(sim), p, llm)
      sondavi_attach(sim, score = verdict$score, feedback = verdict$feedback, persona = p$name, generation = generation)
      verdict$score
    }, 0)
    mean(scores)
  }
  crossover <- function(a, b) {
    cut <- sample(seq_along(RULES), 1)
    list(rules = c(a$rules[seq_len(cut)], b$rules[-seq_len(cut)]),
         temperature = round(mean(c(a$temperature, b$temperature)), 2), count = sample(c(a$count, b$count), 1))
  }
  mutate <- function(g) {
    flip <- runif(length(g$rules)) < mutation
    g$rules[flip] <- 1 - g$rules[flip]
    g$temperature <- round(min(2, max(0, g$temperature + rnorm(1, 0, 0.15))), 2)
    if (runif(1) < mutation) g$count <- min(count_max, max(count_min, g$count + sample(c(-1, 1), 1)))
    g
  }

  pop <- replicate(population, random_genome(), simplify = FALSE)
  fit <- rep(NA_real_, population)
  for (generation in seq_len(generations) - 1) {
    todo <- which(is.na(fit))
    fit[todo] <- vapply(pop[todo], fitness, 0, generation = generation)
    order_ <- order(fit, decreasing = TRUE)
    pop <- pop[order_]; fit <- fit[order_]
    cat(sprintf("generation %d: best %.3f (%s)\n", generation, fit[1],
                paste(names(RULES)[as.logical(pop[[1]]$rules)], collapse = ", ")))
    # The better half survives; the rest is bred from it.
    keep <- seq_len(ceiling(population / 2))
    children <- lapply(seq_len(population - length(keep)), function(i) {
      parents <- sample(keep, 2, replace = length(keep) < 2)
      mutate(crossover(pop[[parents[1]]], pop[[parents[2]]]))
    })
    pop <- c(pop[keep], children)
    fit <- c(fit[keep], rep(NA_real_, length(children)))
  }

  best <- settings_of(pop[[1]])
  cat("\nBest settings (score", round(fit[1], 3), "):\n")
  str(best)
  cat("\nTheir simulations are under run '", run, "' in the survey's Simulations tab, where they can be applied.\n", sep = "")
  invisible(best)
}
