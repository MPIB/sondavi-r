# Plain-R test runner (testthat is not installed on the build host).
#   Rscript tests/run-tests.R
# Expects the fixture server on 127.0.0.1:8765 — see tests/fixture-server.py.

# Installed package if there is one (that is how `R CMD check` runs), otherwise the
# sources next door (that is how one runs it while working on them).
if (!requireNamespace("sondavi", quietly = TRUE)) {
  pkg_root <- if (dir.exists("R")) "." else ".."
  for (f in list.files(file.path(pkg_root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f)
} else {
  library(sondavi)
  # The internals the checks below reach into.
  sondavi_get <- getFromNamespace("sondavi_get", "sondavi")
}

BASE <- "http://127.0.0.1:8765"

# The checks need the fixture server (tests/run.sh starts it). Inside `R CMD check`
# it is not there, and a package whose tests demand a local server would simply be
# unbuildable — so say so and stop, rather than fail.
# An HTTP request, not a raw socket: a bare TCP connect makes the little fixture
# server throw and take the next request with it.
probe <- try(
  httr2::req_perform(httr2::req_error(httr2::req_timeout(httr2::request(BASE), 2),
                                      is_error = function(resp) FALSE)),
  silent = TRUE
)
reachable <- !inherits(probe, "try-error")
if (!reachable) {
  cat("Fixture server not running - skipping. Start it with tests/run.sh\n")
  quit(status = 0)
}

failures <- 0L
checks <- 0L

ok <- function(label, expr) {
  checks <<- checks + 1L
  result <- tryCatch(isTRUE(expr), error = function(e) {
    cat("   error:", conditionMessage(e), "\n"); FALSE
  })
  cat(if (result) "  ✓ " else "  ✗ ", label, "\n", sep = "")
  if (!result) failures <<- failures + 1L
}

fails_with <- function(label, pattern, expr) {
  checks <<- checks + 1L
  msg <- tryCatch({ force(expr); NA_character_ }, error = conditionMessage)
  result <- !is.na(msg) && grepl(pattern, msg)
  cat(if (result) "  ✓ " else "  ✗ ", label, "\n", sep = "")
  if (!result) { failures <<- failures + 1L; cat("   got:", msg, "\n") }
}

TOKEN <- paste0("sdv_", strrep("T", 48))

cat("\nConnecting\n")
fails_with("an empty token explains where it belongs", "Renviron",
           sondavi_connect(BASE, token = ""))
ok("a trailing slash in the address is harmless",
   sondavi_connect(paste0(BASE, "/"), TOKEN)$base_url == BASE)

con <- sondavi_connect(BASE, TOKEN)

cat("\nRefusals speak plainly\n")
fails_with("a bad token names the three indistinguishable cases", "expired or",
           sondavi_surveys(sondavi_connect(BASE, "sdv_wrong")))
fails_with("a study out of scope says so without guessing", "not given access",
           sondavi_responses(con, 999, labels = FALSE))

cat("\nListing and codebook\n")
s <- sondavi_surveys(con)
ok("the studies arrive as a data frame", is.data.frame(s) && nrow(s) == 1)
ok("with the platform's own fields", s$id[1] == 135 && s$response_count[1] == 3)
ok("and the privacy level", s$privacy[1] == "identified")

cb <- sondavi_codebook(con, 135)
ok("the codebook lists every variable", nrow(cb) == 3)
# Measured, not assumed: a `text` question with inputType "number" is classified
# `string` by the platform (SurveyExportService::classifyVariableType matches on the
# SurveyJS type alone). The package must not pretend otherwise — the values still
# arrive as JSON numbers, which is why `age` is numeric below.
ok("it carries the platform's own classification", all(c("string", "categorical") %in% cb$type))
ok("and the value labels of the categorical one",
   nrow(cb$value_labels[[which(cb$name == "mood")]]) == 3)

cat("\nResponses\n")
d <- sondavi_responses(con, 135, labels = FALSE)
ok("every page is walked, not just the first", nrow(d) == 3)
ok("no row is fetched twice", length(unique(d$response_id)) == 3)
ok("the granted identifier is there", "respondent_id" %in% names(d))
ok("the ungranted one is not", !"ip_address" %in% names(d))

cat("\nThe codebook is what makes this better than a CSV\n")
dl <- sondavi_responses(con, 135)
ok("a categorical question becomes a factor", is.factor(dl$mood))
ok("with the real labels, not the codes",
   all(levels(dl$mood) == c("Bad", "Neutral", "Good")))
ok("the first answer reads as its label", as.character(dl$mood[1]) == "Good")
ok("a numeric question is numeric", is.numeric(dl$age))
ok("the question text rides along", attr(dl$age, "label") == "How old are you?")

cat("\nThe fingerprint\n")
fp <- attr(dl, "sondavi_fingerprint")
ok("is attached to the data", !is.null(fp))
ok("and counts the rows actually held, not the first page", fp$rows == 3)
ok("it names the abilities the token had", "identifiers:respondent" %in% fp$abilities)
ok("and prints one line for the paper", grepl("digest", sondavi_fingerprint(dl)))

cat("\nLimits\n")
ok("max_rows stops the walk early", nrow(sondavi_responses(con, 135, max_rows = 1, labels = FALSE)) == 1)

# The platform limits per token and says how long to wait. A client that retries
# without reading Retry-After turns a small limit into a long outage — and this is
# the main reason to ship a package rather than let everyone write their own call.
t0 <- Sys.time()
flaky <- sondavi_get(con, "surveys", list(flaky = 1))
waited <- as.numeric(Sys.time() - t0, units = "secs")
ok("a 429 is waited out rather than hammered", length(flaky$data) == 1 && waited >= 1)

cat("\nCitable datasets\n")
snap <- sondavi_snapshot(con, 135, label = "Paper, figure 2")
ok("a snapshot is cited by a uuid", grepl("^[0-9a-f-]{36}$", snap$id))
ok("it records how many responses were in the set", snap$recorded_rows == 3)
ok("and is complete when nothing has changed", isTRUE(snap$complete))
ok("the label is kept", snap$label == "Paper, figure 2")

rep <- sondavi_snapshot_responses(con, snap$id)
ok("replaying returns the recorded rows", nrow(rep) == 3)
ok("and reports the set as intact", isTRUE(attr(rep, "sondavi_complete")))
ok("with the codebook applied", is.factor(rep$mood))

# The captured answer from AFTER one response was deleted, the way an erasure
# request or retention deletes it. This is what the whole design exists for.
# A separate snapshot id, reached the way a client really builds the path.
ERASED <- "00000000-0000-4000-8000-000000000001"
erased <- withCallingHandlers(
  sondavi_snapshot_responses(con, ERASED, labels = FALSE),
  warning = function(w) { assign("warned", conditionMessage(w), envir = globalenv()); invokeRestart("muffleWarning") }
)
ok("a deleted response shrinks the replay", nrow(erased) == 2)
ok("the set is reported as no longer complete", !isTRUE(attr(erased, "sondavi_complete")))
ok("and the analyst is warned in words", exists("warned") && grepl("deleted since", warned))

ok("the recorded snapshots can be listed", nrow(sondavi_snapshots(con)) >= 1)

cat("\n", checks - failures, "/", checks, " checks passed\n", sep = "")
if (failures) quit(status = 1)
