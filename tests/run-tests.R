# Plain-R test runner (testthat is not installed on the build host).
#   Rscript tests/run-tests.R
# Expects the fixture server — started by tests/run.py, which passes its port.

# Installed package if there is one (that is how `R CMD check` runs), otherwise the
# sources next door (that is how one runs it while working on them).
if (!requireNamespace("sondavi", quietly = TRUE)) {
  pkg_root <- if (dir.exists("R")) "." else ".."
  for (f in list.files(file.path(pkg_root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f)
} else {
  library(sondavi)
  # The internals the checks below reach into.
  sondavi_get <- getFromNamespace("sondavi_get", "sondavi")
  apply_codebook <- getFromNamespace("apply_codebook", "sondavi")
  parse_status <- getFromNamespace("parse_status", "sondavi")
}

# Der Port kommt aus tests/run.py, das einen freien waehlt: ein fester ist eine Wette auf
# die Maschine, und CI-Laeufer bringen eigene Dienste mit.
PORT <- Sys.getenv("SONDAVI_TEST_PORT", "8765")
BASE <- paste0("http://127.0.0.1:", PORT)

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
ok("the studies arrive as a data frame", is.data.frame(s) && nrow(s) == 4)
# Not a written-down id: `clients/fixtures-aufzeichnen.sh` records a fresh study each
# time, so a constant here would fail the suite for a reason that has nothing to do
# with the client.
SURVEY <- s$id[1]
ok("with the platform's own fields", is.numeric(SURVEY) && s$response_count[1] == 3)
ok("and the privacy level", s$privacy[1] == "identified")

cb <- sondavi_codebook(con, SURVEY)
# Named, not counted: adding a question to the recorded study should not fail a test
# about the codebook arriving at all.
ok("the codebook lists every variable",
   all(c("age", "mood", "why") %in% cb$name) && "ratings.speed.score" %in% cb$name)
# Measured, not assumed: a `text` question with inputType "number" is classified
# `string` by the platform (SurveyExportService::classifyVariableType matches on the
# SurveyJS type alone). The package must not pretend otherwise — the values still
# arrive as JSON numbers, which is why `age` is numeric below.
ok("it carries the platform's own classification", all(c("string", "categorical") %in% cb$type))
ok("and the value labels of the categorical one",
   nrow(cb$value_labels[[which(cb$name == "mood")]]) == 3)

# The API names the range a marked row is mirrored in; the copy itself is the client's to make.
worry <- cb[cb$name == "b5.worry", ]
calm <- cb[cb$name == "b5.calm", ]
ok("a matrix row marked reverse-scored says so", isTRUE(worry$reverse_scored))
ok("with the range of its scale", worry$reverse_min == 1 && worry$reverse_max == 5)
ok("an unmarked row has neither", !calm$reverse_scored && is.na(calm$reverse_min) && is.na(calm$reverse_max))
ok("the reversed copy is not a variable of the API", !"b5.worry_r" %in% cb$name)

cat("\nResponses\n")
d <- sondavi_responses(con, SURVEY, labels = FALSE)
ok("every page is walked, not just the first", nrow(d) == 3)
ok("no row is fetched twice", length(unique(d$response_id)) == 3)
ok("the granted identifier is there", "respondent_id" %in% names(d))
ok("the ungranted one is not", !"ip_address" %in% names(d))
ok("the platform's internal fields come under their own names", all(d$`__locale` == "default"))
ok("raw values fetch no codebook", is.null(attr(d, "sondavi_codebook")))

cat("\nThe codebook is what makes this better than a CSV\n")
dl <- sondavi_responses(con, SURVEY)
ok("a categorical question becomes a factor", is.factor(dl$mood))
ok("with the real labels, not the codes",
   all(levels(dl$mood) == c("Bad", "Neutral", "Good")))
ok("the first answer reads as its label", as.character(dl$mood[1]) == "Good")
ok("a numeric question is numeric", is.numeric(dl$age))
ok("the question text rides along", attr(dl$age, "label") == "How old are you?")
ok("the codebook it applied is kept with the data", identical(attr(dl, "sondavi_codebook")$name, cb$name))

# Timestamps as timestamps. As text they are a quiet trap: comparing a character
# column against a date returns an answer instead of an error, and sorting by it
# sorts lexically. Both reach a result without ever looking wrong.
ok("completed_at is a point in time, not text", inherits(dl$completed_at, "POSIXct"))
ok("started_at too", inherits(dl$started_at, "POSIXct"))
ok("the platform's time zone is honoured",
   identical(attr(dl$completed_at, "tzone"), "UTC"))
ok("so a duration can simply be computed",
   all(as.numeric(difftime(dl$completed_at, dl$started_at, units = "mins")) > 0))

cat("\nThe fingerprint\n")
fp <- attr(dl, "sondavi_fingerprint")
ok("is attached to the data", !is.null(fp))
ok("and counts the rows actually held, not the first page", fp$rows == 3)
ok("it names the abilities the token had", "identifiers:respondent" %in% fp$abilities)
ok("and prints one line for the paper", grepl("digest", sondavi_fingerprint(dl)))

cat("\nLimits\n")
ok("max_rows stops the walk early", nrow(sondavi_responses(con, SURVEY, max_rows = 1, labels = FALSE)) == 1)

# The platform limits per token and says how long to wait. A client that retries
# without reading Retry-After turns a small limit into a long outage — and this is
# the main reason to ship a package rather than let everyone write their own call.
t0 <- Sys.time()
flaky <- sondavi_get(con, "surveys", list(flaky = 1))
waited <- as.numeric(Sys.time() - t0, units = "secs")
# Counted against the study list rather than a literal: what this asserts is that the real
# answer arrives after the wait, not how many studies the recording happens to hold.
ok("a 429 is waited out rather than hammered", length(flaky$data) == nrow(s) && waited >= 1)

cat("\nCitable datasets\n")
snap <- sondavi_snapshot(con, SURVEY, label = "Paper, figure 2")
ok("a snapshot is cited by a uuid", grepl("^[0-9a-f-]{36}$", snap$id))
ok("it records how many responses were in the set", snap$recorded_rows == 3)
ok("and is complete when nothing has changed", isTRUE(snap$complete))
ok("the label is kept", snap$label == "Paper, figure 2")

rep <- sondavi_snapshot_responses(con, snap$id)
ok("replaying returns the recorded rows", nrow(rep) == 3)
ok("and reports the set as intact", isTRUE(attr(rep, "sondavi_complete")))
ok("with the codebook applied", is.factor(rep$mood))
ok("and the status as a factor, as in sondavi_responses()", identical(levels(rep$response_status), c("complete", "partial", "screened_out")))

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

# A token that reads FEWER columns than the snapshot was recorded with. Its digest
# cannot match by design - the rows are deliberately narrower - and reporting that as
# "the dataset has changed" is a false alarm. Measured against the running platform:
# it did exactly that, and a warning that fires when nothing is wrong teaches the
# analyst to ignore the one case it exists for.
NARROWED <- "00000000-0000-4000-8000-000000000002"
narrow_warning <- NULL
narrow_note <- NULL
narrowed <- withCallingHandlers(
  sondavi_snapshot_responses(con, NARROWED, labels = FALSE),
  warning = function(w) { narrow_warning <<- conditionMessage(w); invokeRestart("muffleWarning") },
  message = function(m) { narrow_note <<- conditionMessage(m); invokeRestart("muffleMessage") }
)
ok("a narrowed replay still returns every recorded row", nrow(narrowed) == 3)
ok("and is not reported as a deletion", is.null(narrow_warning))
ok("the set counts as complete", isTRUE(attr(narrowed, "sondavi_complete")))
ok("the reader is told why the fingerprint does not apply", !is.null(narrow_note))
ok("and that it was read differently than recorded",
   !isTRUE(attr(narrowed, "sondavi_read_as_recorded")))

ok("the recorded snapshots can be listed", nrow(sondavi_snapshots(con)) >= 1)

cat("\nNested answers\n")
# The recording holds an answer whose option was deleted afterwards; what unnest says about it
# is checked below, so it is collected here rather than printed.
unnest_warnings <- character()
flat <- withCallingHandlers(sondavi_unnest(dl), warning = function(w) {
  unnest_warnings <<- c(unnest_warnings, conditionMessage(w))
  invokeRestart("muffleWarning")
})
# The names must be the platform's own, or a script written against the export file and one
# written against the API disagree about what a variable is called.
ok("a matrix cell becomes question.row.column", "ratings.speed.score" %in% names(flat))
ok("with the recorded value", flat$ratings.speed.score[1] == 4)
ok("a dynamic panel entry counts from zero, as the export does",
   "contacts.0.who" %in% names(flat))
ok("someone who named fewer entries gets NA, not a shifted row",
   is.na(flat$contacts.1.who[2]))
ok("the nested column itself is gone", !"ratings" %in% names(flat))
ok("the plain columns are untouched", identical(as.character(flat$mood), as.character(dl$mood)))
ok("and the fingerprint survives", !is.null(attr(flat, "sondavi_fingerprint")))
ok("naming one column leaves the others nested",
   is.list(sondavi_unnest(dl, columns = "ratings")$contacts))

# Every name unnest produces must be one the codebook knows: that is the coupling worth a test,
# because both sides derive it separately.
cb_names <- cb$name[!is.na(cb$name)]
produced <- grep("^(ratings|contacts|map|visits)\\.", names(flat), value = TRUE)
# A reversed copy is not a variable of the API's codebook; it is built from its item's row.
copies <- paste0(cb$name[cb$reverse_scored & !is.na(cb$reverse_min)], "_r")
ok("every flattened name appears in the codebook (a reversed copy through its item)",
   all(produced %in% c(cb_names, copies)))
ok("the internal fields pass through untouched", all(flat$`__locale` == "default"))
ok("the codebook stays with the data", !is.null(attr(flat, "sondavi_codebook")))

cat("\nMatrix cells get the codebook too\n")
# It used to be applied before unnesting only, so a matrix cell stayed a bare code next to a
# question that is a factor.
ok("a matrix cell with value labels becomes a factor", is.factor(flat$b5.worry))
ok("reading as its label", as.character(flat$b5.worry[1]) == "A little")
ok("an off-scale option keeps its own label", as.character(flat$b5.worry[2]) == "Don't know")
ok("a numeric cell is a number", is.numeric(flat$ratings.speed.score))
ok("and each cell carries its question text", grepl("Speed", attr(flat$ratings.speed.score, "label")))

cat("\nReverse-scored items\n")
at <- match("b5.worry", names(flat))
ok("the reversed copy follows its item, as in the export file", identical(names(flat)[at + 1], "b5.worry_r"))
ok("it is a number, not a factor", is.numeric(flat$b5.worry_r))
# Computed from the answer as given (2), not from the factor it became ("A little", code 2 by
# accident here - which is exactly why the order matters).
ok("min + max - answer: 2 on a 1-5 scale becomes 4", flat$b5.worry_r[1] == 4)
ok("an answer off the scale (dk) has no reversed value", is.na(flat$b5.worry_r[2]))
ok("its label says what it is", grepl("(reverse-scored)", attr(flat$b5.worry_r, "label"), fixed = TRUE))
ok("an unmarked row gets no copy", !"b5.calm_r" %in% names(flat))
replayed_flat <- suppressWarnings(sondavi_unnest(rep))
ok("a replayed snapshot carries its codebook too", replayed_flat$b5.worry_r[3] == 1)
raw_flat <- sondavi_unnest(dl, codebook = NULL)
ok("codebook = NULL leaves the cells as stored",
   identical(raw_flat$b5.worry, c("2", "dk", "2")) && !"b5.worry_r" %in% names(raw_flat))
raw_unlabelled <- sondavi_unnest(d)
ok("and so does unnesting data read with labels = FALSE",
   !is.factor(raw_unlabelled$b5.worry) && !"b5.worry_r" %in% names(raw_unlabelled))
ok("a reverse-scored row of a multi-select matrix gets its copy as well",
   identical(names(flat)[match("ratings.clarity.score", names(flat)) + 1], "ratings.clarity.score_r") &&
   flat$ratings.clarity.score_r[1] == 1)

cat("\nThe reversed copies are the platform's own\n")
# The platform's "JSON (flat)" export of the same study, recorded next to the API answers
# (clients/fixtures-aufzeichnen.sh). The package builds `_r` itself from the codebook; this
# checks that it builds the same columns with the same values, rather than asserting it.
FIXTURES <- local({
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1]
  file.path(dirname(normalizePath(script)), "fixtures")
})
exported <- jsonlite::fromJSON(file.path(FIXTURES, "export_flat.json"), simplifyVector = FALSE)
export_copies <- grep("_r$", names(exported[[1]]), value = TRUE)
# The snapshot replay holds all three recorded rows with their real ids.
built <- replayed_flat
ok("the export file has reversed copies to compare with", length(export_copies) >= 2)
ok("the package builds exactly the export's reversed columns",
   setequal(export_copies, grep("_r$", names(built), value = TRUE)))
same_values <- vapply(export_copies, function(col) {
  want <- vapply(exported, function(r) if (is.null(r[[col]]) || r[[col]] == "") NA_real_ else as.numeric(r[[col]]), numeric(1))
  ids <- vapply(exported, function(r) as.numeric(r$response_id), numeric(1))
  got <- built[[col]][match(ids, built$response_id)]
  identical(as.numeric(got), want)
}, logical(1))
ok("with the export's values, row by row (\"dk\" empty in both)", all(same_values))

cat("\nScale scores\n")
# as.numeric() on a factor is the position of the level: "Don't know", the sixth option of
# b5.worry, becomes 6. The scale functions read the numbers from the codebook instead.
scale_items <- c("b5.calm", "b5.worry")
it <- sondavi_items(flat, scale_items)
ok("the items come as numbers, a reverse-scored one under its copy's name",
   identical(names(it), c("b5.calm", "b5.worry_r")) && all(vapply(it, is.numeric, logical(1))))
ok("reversed from the answer: 2 becomes 4", it$b5.worry_r[1] == 4 && it$b5.calm[1] == 4)
ok("\"Don't know\" is no number, not 6", as.numeric(flat$b5.worry[2]) == 6 && is.na(it$b5.worry_r[2]))
ok("an answer without a label keeps its value (6 for a deleted option)", it$b5.calm[2] == 6)
ok("reverse = FALSE gives the item as answered",
   identical(names(sondavi_items(flat, scale_items, reverse = FALSE)), scale_items) &&
   sondavi_items(flat, scale_items, reverse = FALSE)$b5.worry[1] == 2)
ok("the score is the mean of the items", sondavi_scale(flat, scale_items)[1] == 4)
ok("by default every item has to be answered", is.na(sondavi_scale(flat, scale_items)[2]))
ok("min_items allows gaps on purpose", sondavi_scale(flat, scale_items, min_items = 1)[2] == 6)
ok("data read with labels = FALSE give the same numbers with the codebook passed",
   isTRUE(all.equal(sondavi_items(raw_unlabelled, scale_items, codebook = cb), it)))
ok("without a codebook it says how to get one",
   inherits(tryCatch(sondavi_items(raw_unlabelled, scale_items), error = function(e) e), "error") &&
   grepl("sondavi_codebook", tryCatch(sondavi_items(raw_unlabelled, scale_items), error = conditionMessage)))
ok("an unknown column is named", grepl("b5.nope", tryCatch(sondavi_items(flat, "b5.nope"), error = conditionMessage)))

cat("\nAnswers without a value label\n")
# Matching against the labels alone turned them into NA, which reads as "not answered".
calm_warnings <- grep("b5.calm", unnest_warnings, fixed = TRUE, value = TRUE)
ok("are kept, named by their value", as.character(flat$b5.calm[2]) == "6")
ok("as a level after the labelled ones",
   identical(levels(flat$b5.calm), c("Not at all", "A little", "Somewhat", "Very", "Completely", "Don't know", "6")))
ok("the analyst is warned once for the column, with the value",
   length(calm_warnings) == 1 && grepl('"6"', calm_warnings, fixed = TRUE))
ok("a column whose answers all have labels is not mentioned",
   !any(grepl("b5.worry", unnest_warnings, fixed = TRUE)))

cat("\nThe same rules on answers the recording does not have\n")
labels_of <- function(value, text) data.frame(value = value, text = text, stringsAsFactors = FALSE)
synthetic_cb <- data.frame(
  name = c("q.a", "q.a_r", "consent"), label = c("Item A", "Another variable", "Do you agree?"),
  type = c("categorical", "numeric", "categorical"), surveyjs_type = c("matrix", "text", "boolean"),
  reverse_scored = c(TRUE, FALSE, FALSE), reverse_min = c(1, NA, NA), reverse_max = c(7, NA, NA),
  stringsAsFactors = FALSE
)
synthetic_cb$value_labels <- list(
  labels_of(as.character(1:7), paste("Level", 1:7)),
  labels_of(character(), character()),
  labels_of(c("true", "false"), c("Yes", "No"))
)
nested_in <- data.frame(id = 1:3)
nested_in$q <- I(list(list(a = 3, a_r = 9), list(a = "7"), list(a = "0x1A")))
synthetic_warnings <- character()
nested_out <- withCallingHandlers(sondavi_unnest(nested_in, codebook = synthetic_cb), warning = function(w) {
  synthetic_warnings <<- c(synthetic_warnings, conditionMessage(w))
  invokeRestart("muffleWarning")
})
# The platform's naming rule: a copy never takes the name of an existing variable.
ok("a taken name makes the copy <name>_r2",
   identical(names(nested_out), c("id", "q.a", "q.a_r2", "q.a_r")))
ok("and the variable that had the name keeps its values", identical(as.numeric(nested_out$q.a_r), c(9, NA, NA)))
ok("a number given as text is still a number", nested_out$q.a_r2[2] == 1)
# What counts as a number is PHP's is_numeric, as on the platform; as.numeric() would read 26.
ok("hexadecimal is not", is.na(nested_out$q.a_r2[3]))
ok("its unlabelled value is named in the warning", any(grepl('"0x1A"', synthetic_warnings, fixed = TRUE)))

top_warnings <- character()
top <- withCallingHandlers(
  apply_codebook(data.frame(q.a = c(2, NA, 5), consent = c(TRUE, FALSE, NA), q.a_r = c("9", "dk", NA),
                            stringsAsFactors = FALSE), synthetic_cb),
  warning = function(w) { top_warnings <<- c(top_warnings, conditionMessage(w)); invokeRestart("muffleWarning") }
)
ok("a reverse-scored column at the top level gets its copy as well",
   identical(names(top)[1:2], c("q.a", "q.a_r2")) && identical(as.numeric(top$q.a_r2), c(6, NA, 3)))
# The codebook writes yes/no as "true"/"false"; matched as "TRUE", every one became NA.
ok("a yes/no answer reads as its label", identical(as.character(top$consent), c("Yes", "No", NA)))
ok("a numeric variable says which answers are no numbers",
   any(grepl("`q.a_r`", top_warnings, fixed = TRUE) & grepl('"dk"', top_warnings, fixed = TRUE)))

cat("\nImage marking\n")
# The API sends the stored answer (image, grid, cells or pins); unnest has to write it the way
# the export does - one column per marking type - not as a tree of image.src, grid.cols, ...
ok("an area answer becomes one column per marking type",
   all(c("map.green", "map.red") %in% names(flat)))
ok("none of the stored structure leaks out as columns",
   !any(grepl("^(map|visits)\\.(mode|image|grid|cells|points)", names(flat))))
ok("painted cells are written as row runs, like the export", flat$map.green[1] == "2:3-5 3:4")
ok("a single cell has no dash", flat$map.red[1] == "6:4")
ok("a type nobody used in this answer is NA", is.na(flat$map.red[2]))
ok("pins are x,y pairs in the order set", flat$visits.visit[1] == "0.25,0.5 0.7,0.1234")
ok("someone who set no pins gets NA", is.na(flat$visits.visit[2]))

m <- sondavi_markings(dl)
ok("the long table has one row per cell and per pin", nrow(m) == 4 + 1 + 1 + 2)
ok("with the columns of the export's markings.csv",
   identical(names(m), c("response_id", "respondent_id", "completed_at", "question", "category",
                         "row", "col", "x_norm", "y_norm", "x_px", "y_px", "image_src")))
cell <- m[m$question == "map" & m$category == "green", ][1, ]
ok("a cell is named by row and column, counted from 0", cell$row == 2 && cell$col == 3)
# The centre of cell (2, 3) on a 16 x 11 grid over 1600 x 1100 px.
ok("placed at its centre, relative to the image", abs(cell$x_norm - 3.5 / 16) < 1e-6 && abs(cell$y_norm - 2.5 / 11) < 1e-6)
ok("and in pixels of the original image", cell$x_px == 350 && cell$y_px == 250)
pin <- m[m$question == "visits", ][2, ]
ok("a pin has no cell", is.na(pin$row) && is.na(pin$col))
ok("but both coordinates", pin$x_norm == 0.7 && pin$y_px == round(0.1234 * 1100, 2))
ok("each row says which image it was drawn on", all(m$image_src == "/storage/test/TEST-map.png"))
ok("and whose answer it is", all(m$respondent_id[m$question == "visits"] == "PNL-1"))
ok("one question can be asked for", all(sondavi_markings(dl, columns = "visits")$question == "visits"))
none <- sondavi_markings(dl[, setdiff(names(dl), c("map", "visits"))])
ok("a study without image marking gives an empty table, same columns",
   nrow(none) == 0 && identical(names(none), names(m)))

cat("\nAI interviews\n")
# An interview question arrives as the whole conversation (mode, status, turns, messages, ...).
# unnest writes the export's columns - the ones the codebook lists - not a column per message;
# sondavi_conversations() is the conversation as a long table. The study is a real recording.
di <- sondavi_responses(con, s$id[3])
fi <- sondavi_unnest(di)
ok("an interview question becomes the export's columns",
   all(c("talk.status", "talk.turns", "talk.answered", "talk.skipped", "talk.summary", "chat.status", "chat.turns") %in% names(fi)))
ok("and nothing of the stored conversation leaks out as columns",
   !any(grepl("^(talk|chat)\\.(mode|messages|questionStatus|microNarrative)", names(fi))))
ok("every column the codebook names for it is there",
   all(grep("^(talk|chat)\\.", attr(di, "sondavi_codebook")$name, value = TRUE) %in% names(fi)))
ok("a free interview has no counts of questions", !any(c("chat.answered", "chat.skipped") %in% names(fi)))
ok("how it ended is a factor with the codebook's labels",
   is.factor(fi$talk.status) && identical(as.character(fi$talk.status[1:2]), c("Completed", "Ended early after technical problems")))
ok("turns and question counts are numbers",
   identical(as.numeric(fi$talk.turns[1:2]), c(4, 1)) && identical(as.numeric(fi$talk.answered[1:2]), c(2, 1)) &&
   identical(as.numeric(fi$talk.skipped[1:2]), c(0, 0)))
ok("the chosen summary is there, NA where none was chosen",
   startsWith(fi$talk.summary[1], "I write software") && is.na(fi$talk.summary[2]))
ok("someone who never met the question has NA", is.na(fi$chat.status[2]) && is.na(fi$talk.status[3]))
ok("the whole-survey interview's probing stays readable",
   isTRUE(fi$interview_probing.job.was_probed[3]) && grepl("^I: What is your job", fi$interview_probing.job.transcript[3]))

talk <- sondavi_conversations(di)
ok("the conversations: one row per message", nrow(talk) == 9 + 7 + 2)
ok("with their columns", identical(names(talk), c("response_id", "respondent_id", "completed_at", "question", "mode",
                                                  "message", "role", "answer", "text", "input", "at", "state")))
one <- talk[talk$question == "talk" & talk$respondent_id == "PNL-1", ]
ok("in the order of the conversation, interviewer first", identical(one$message, seq_len(9)) && one$role[1] == "interviewer")
ok("the respondent's answers are counted", identical(one$answer[one$role == "respondent"], 1:4) && all(is.na(one$answer[one$role == "interviewer"])))
ok("an answer says whether it was typed or spoken",
   identical(talk$input[talk$question == "chat" & talk$role == "respondent"], c("text", "voice", "text")))
ok("each message has its time, in UTC", inherits(talk$at, "POSIXct") && !anyNA(talk$at) && attr(talk$at, "tzone") == "UTC")
ok("and its words", grepl("^TEST I write software", one$text[2]))
ok("an interview that ended early keeps what was said", sum(talk$respondent_id == "PNL-2") == 2)
ok("one question can be asked for", all(sondavi_conversations(di, columns = "chat")$question == "chat"))
# What the recording does not have: a flow interview, a summary in a free one, a blank choice.
flow <- list(mode = "flow", status = "active", turns = 1L, messages = list(
  list(role = "assistant", text = "Hi", at = "2026-10-15T20:00:00.000000Z", state = "intro"),
  list(role = "user", text = "Hello", at = "2026-10-15T20:00:05.500000Z", input = "voice")),
  statePath = list(list(state = "intro", via = "start", turn = 0L)))
free <- list(mode = "free", status = "completed", turns = 2L, messages = list(),
             microNarrative = list(status = "chosen", options = list("  "), chosenIndex = 0L, chosen = "  "))
own <- data.frame(response_id = 1:2)
own$q <- I(list(flow, free))
of <- sondavi_unnest(own, codebook = NULL)
ok("a flow interview has no counts of questions and no summary", !any(c("q.answered", "q.skipped") %in% names(of)) &&
   identical(of$q.status, c("active", "completed")))
ok("a free interview offering summaries has the summary column; a blank choice is none",
   "q.summary" %in% names(of) && all(is.na(of$q.summary)))
ok("an interview without summaries gets no summary column", !"q.summary" %in% names(sondavi_unnest(own[1, ], codebook = NULL)))
covered <- list(mode = "state", status = "completed", turns = 1L, messages = list(), questionStatus = list(
  list(id = "a", status = "answered"), list(id = "b", status = "skipped_anticipated"), list(id = "c", status = "pending")))
cv <- data.frame(response_id = 3L)
cv$q <- I(list(covered))
cf <- sondavi_unnest(cv, codebook = NULL)
ok("a question already covered by an earlier answer counts as skipped, an open one as neither",
   cf$q.answered == 1 && cf$q.skipped == 1)
oc <- sondavi_conversations(own)
ok("a flow interview's messages say in which stage the interviewer was", identical(oc$state, c("intro", NA)))
ok("time to the second, fractions kept", abs(as.numeric(difftime(oc$at[2], oc$at[1], units = "secs")) - 5.5) < 1e-6)
ok("an interview without messages contributes no rows", all(oc$response_id == 1))
ok("a study without interview questions gives an empty table, same columns",
   nrow(sondavi_conversations(dl)) == 0 && identical(names(sondavi_conversations(dl)), names(talk)))

cat("\nScreened out, and a bot check as a question\n")
# The fourth study screens people under 18 out and asks a bot check on its second page
# (SPECS §79, §80): PNL-2 was screened out, PNL-3 failed the check, which only flags.
sc <- sondavi_responses(con, s$id[4])
ok("everyone is a row, the screened-out person too", nrow(sc) == 3)
ok("the screened-out row has no completed_at", identical(is.na(sc$completed_at), c(FALSE, TRUE, FALSE)))
ok("its time of screen-out is a timestamp, like completed_at",
   inherits(sc$screened_out_at, "POSIXct") && identical(is.na(sc$screened_out_at), c(TRUE, FALSE, TRUE)))
ok("and its reason is there", identical(sc$screen_out_reason, c(NA, "under 18", NA)))
ok("finished means completed_at set: the screen-out is not counted", nrow(subset(sc, !is.na(completed_at))) == 2)
ok("every row says what became of it: a factor complete / partial / screened_out, all three levels",
   is.factor(sc$response_status) && identical(levels(sc$response_status), c("complete", "partial", "screened_out")) &&
     identical(as.character(sc$response_status), c("complete", "screened_out", "complete")))
ok("and counts the completed ones as completed_at does", nrow(subset(sc, response_status == "complete")) == 2)
ok("table() names the empty level too", identical(as.vector(table(sc$response_status)), c(2L, 0L, 1L)))
said <- NULL
st <- withCallingHandlers(parse_status(data.frame(response_status = c("complete", "quota_full"), stringsAsFactors = FALSE)),
                          warning = function(w) { said <<- conditionMessage(w); invokeRestart("muffleWarning") })
ok("a status this version does not know is kept as a level of its own, with a warning",
   identical(as.character(st$response_status), c("complete", "quota_full")) && grepl("quota_full", said))
ok("the bot check question reads as passed / failed, with its labels",
   is.factor(sc$human) && identical(as.character(sc$human), c("Passed", NA, "Failed")))
ok("the overall bot check result is a yes/no column", identical(sc$bot_check_passed, c(TRUE, NA, FALSE)))

cat("\nJoining waves\n")
waves <- sondavi_waves(con, s$id[1:2], names = c("w1", "w2"))
ok("one row per person seen in any wave", nrow(waves) == 3)
ok("the columns carry their wave", all(c("mood_w1", "mood_w2") %in% names(waves)))
ok("the join key is not suffixed", "respondent_id" %in% names(waves))
# The point of the full outer join: attrition is usually the thing being studied, so the
# person who stopped answering must not quietly disappear from the table.
dropped <- waves[is.na(waves$mood_w2), ]
ok("someone who skipped the second wave is kept", nrow(dropped) == 1)
ok("and is recognisable by their missing wave", dropped$respondent_id == "PNL-3")

joined_err <- tryCatch(sondavi_waves(con, s$id[1]), error = function(e) conditionMessage(e))
ok("one study is not a series", grepl("at least two", joined_err))

cat("\n", checks - failures, "/", checks, " checks passed\n", sep = "")
if (failures) quit(status = 1)
