# sondavi

Read your own study's data from the [Sondavi](https://github.com/MPIB) survey platform
directly into R, instead of exporting a file first.

```r
library(sondavi)

con <- sondavi_connect("https://survey.example.org")   # token from SONDAVI_TOKEN

sondavi_surveys(con)
#>   id             title project    status      privacy response_count
#> 1 42  Wave 2 follow-up  Ageing published pseudonymized            318

d <- sondavi_responses(con, 42)
levels(d$mood)
#> [1] "Bad" "Neutral" "Good"
```

## Installation

```r
# install.packages("remotes")
remotes::install_github("MPIB/sondavi-r")
```

## The token

Create one under your account on the platform, then put it in `~/.Renviron`:

```
SONDAVI_TOKEN=sdv_…
```

and restart the R session. **Not in the analysis script.** A token written into a script
travels with it into version control, onto shared drives and into supplementary material —
that is the usual way one leaks.

A token reads only, covers only the studies you chose for it, and expires by itself after
at most 90 days.

## What you get

| | |
|---|---|
| `sondavi_surveys(con)` | the studies this token may read |
| `sondavi_codebook(con, id)` | variable names, labels, types, value labels |
| `sondavi_responses(con, id)` | the responses, all pages, codebook applied |
| `sondavi_snapshot(con, id)` | record a citable dataset |
| `sondavi_snapshot_responses(con, snapshot_id)` | replay one |
| `sondavi_fingerprint(d)` | the line that belongs in your paper |
| `sondavi_unnest(d)` | spread matrices and dynamic panels into one column each |
| `sondavi_items(flat, items)` / `sondavi_scale(flat, items)` | the items of a scale as numbers, and the scale score |
| `sondavi_markings(d)` | image marking answers as one row per cell or pin, for a heatmap |
| `sondavi_conversations(d)` | AI interview questions as one row per message |
| `sondavi_waves(con, ids)` | join the waves of a study series by respondent |

`sondavi_responses()` applies the codebook by default, so a categorical question arrives as
a factor with its real labels rather than as bare codes, and the question text rides along
as a `label` attribute. Pass `labels = FALSE` for the raw values.

`completed_at` and `started_at` (and `screened_out_at`, in a study that screens people out)
arrive as real `POSIXct` timestamps in the platform's time zone, so a duration is
`difftime(d$completed_at, d$started_at)` and not a parsing exercise.

These are the same rows as the platform's JSON export, which means **partial responses are
included** when the study saves them — someone who stopped halfway is a row whose
`completed_at` is `NA`. So is someone the study screened out; their row carries
`screened_out_at` and `screen_out_reason` instead. `nrow(d)` is therefore not the number of
completed participations. Every row says what became of it in `response_status`, a factor
with the levels `complete`, `partial` and `screened_out`:

```r
table(d$response_status)
done <- subset(d, response_status == "complete")
```

Nested answers — matrices, dynamic panels, jsPsych trials — stay as list columns. Flattening
them here would invent a shape the platform did not give. When you do want the flat form,
`sondavi_unnest()` produces exactly the column names the platform's own export writes:

```r
flat <- sondavi_unnest(d)
names(flat)
#> … "ratings.speed.score" "ratings.clarity.score" "contacts.0.who"
```

The new columns get the codebook too: a matrix cell with value labels is a factor, like
the questions beside it. A matrix row marked "Reverse-scored" in the editor gets its
reversed copy right after it, as in the platform's export file — `b5.worry_r` =
(smallest + largest scale value) − answer, a number, `NA` for an answer off the scale
such as "don't know".

A value the codebook has no label for (an answer to an option deleted later) is kept as
a factor level of its own, named by the value, with a warning — not turned into `NA`.

## Scale scores

A factor is right for a table, wrong for arithmetic: `as.numeric()` gives the position of
the level, not the answer — on a 1–5 scale with "Don't know" as a sixth option, "Don't
know" becomes 6, and the score is off without a warning. Name the items as they are called
in the questionnaire instead; the package takes their numbers from the codebook and uses
the reversed copy for every item marked "Reverse-scored", so you need not remember which
ones are worded the other way round:

```r
items <- c("b5.calm", "b5.worry", "b5.rested")         # b5.worry is reverse-scored
flat$b5_score <- sondavi_scale(flat, items)             # NA unless all three are answered
flat$b5_score <- sondavi_scale(flat, items, min_items = 2)   # allow one gap, on purpose
psych::alpha(sondavi_items(flat, items))                # b5.calm, b5.worry_r, b5.rested
```

"Don't know" and other answers that are no number count as missing. By default every item
has to be answered, so a mean over fewer items never happens unnoticed.

## Image marking

An image marking question (participants paint areas or set pins on a map or picture) arrives
as the stored answer — the image, the grid, and the cells or pins. For a heatmap:

```r
m <- sondavi_markings(d)
green <- subset(m, question == "map" & category == "green")
table(green$row, green$col)          # how many people marked each cell
```

One row per painted cell or pin, with its position between 0 and 1 (`x_norm`, `y_norm`) and in
pixels of the original image (`x_px`, `y_px`) — the same table as the platform's
image-markings export. `sondavi_unnest()` writes these questions the way the CSV export does:
one column per marking type, cells as row runs (`"2:3-5 3:4"`), pins as `"x,y"` pairs.

## AI interviews

An AI interview question (an interview conducted by a language model inside the questionnaire)
arrives as the whole conversation. `sondavi_unnest()` writes the columns the CSV export and the
codebook have — `<question>.status` (how it ended, a factor), `.turns` (answers given), for an
interview with a list of questions `.answered` and `.skipped` (already covered by an earlier
answer), and `.summary` when the interview offers summaries. The conversation itself, one row
per message:

```r
talk <- sondavi_conversations(d)     # before sondavi_unnest()
said <- subset(talk, role == "respondent")
tapply(said$text, said$response_id, paste, collapse = " ")   # all a person said
table(said$input)                                            # typed or spoken
```

Each row has `question`, `message` (its place in the conversation), `role` (interviewer or
respondent), `answer` (the respondent's 1st, 2nd … answer), `text`, `input` (`"text"` or
`"voice"`) and `at` (when it was written, UTC); a flow interview's messages also name the
stage (`state`).

A study run entirely as an interview (the whole questionnaire as a chat) stores the answers
as usual, plus per question whether and how often the interviewer followed up and the
exchange about it as text: `sondavi_unnest()` gives `interview_probing.<question>.was_probed`,
`.follow_up_count`, `.initial_response` and `.transcript`. The complete conversation of such
an interview, with recordings, is in the platform's interview-transcript export.

## Waves of a study series

```r
d <- sondavi_waves(con, c(42, 43), names = c("baseline", "followup"))
table(returned = !is.na(d$mood_followup))
```

Columns are suffixed per wave, and **everyone seen in any wave is kept** — a person who
did not take the follow-up is still a row, with `NA` in its columns. Attrition is usually
what a longitudinal design is about, so an inner join would drop exactly the cases you
want to describe.

## Fetching only what is new

```r
d <- sondavi_responses(con, 42, since = Sys.Date() - 7)
```

## A live query is not a dataset

Run the same script tomorrow and it may return different rows — new responses arrive, and
retention or a data-subject request may remove some. Either record the fingerprint:

```r
sondavi_fingerprint(d)
#> survey 42, 318 rows, fetched 2026-09-23T09:32:29+00:00, digest 332f3d2a1fbfd880
```

or record a citable dataset:

```r
snap <- sondavi_snapshot(con, 42, label = "Paper, figure 2")
d    <- sondavi_snapshot_responses(con, snap$id)
```

A snapshot records *which* responses your result was computed from, never a copy of them —
so deletion requests and retention keep working. Replay it later and, if some are gone, you
get a warning saying how many, rather than a quietly smaller dataset.

## Identifiers

Answers always come through. Two groups only if the token was given those abilities when you
created it: the **participant identifier**, and the **technical data** — the IP address plus
the device traits a study records if it collects them (browser, operating system, screen
resolution, window size, time zone, browser language), which together identify a device
fairly well. Neither ever goes beyond what the study's own privacy level allows: an anonymous
study returns no identifier whatever the token says.

Everything else is your study's own data and always arrives — which experimental group
someone was in, their language, the order questions were shown in.

## The whole thing, end to end

```r
vignette("sondavi")
```

## Tests

```
tests/run.sh
```

Starts a small server that replays **real answers captured from the platform**
(`tests/fixtures/`), including one recorded after a response was erased. No network, no
credentials. Under `R CMD check` these are skipped, since they need that server.

## License

MIT
