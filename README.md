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
| `sondavi_verify(data, certificate)` | check a certified dataset against its certificate |

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

## Verifying a certified dataset

The platform can issue a dataset with a certificate: `wave2.csv` plus
`wave2.certificate.json`, signed by the platform. It vouches that every cell it covers
holds exactly what the participants entered — so a reviewer, a journal or a co-author can
check the data themselves, without an account and without having to trust whoever sent
the files.

```r
v <- sondavi_verify("wave2.csv", "wave2.certificate.json")
v
#> Valid: every certified cell matches.
#> Survey:          Wave 2 follow-up (id 42, questionnaire version 3, pseudonymized)
#> Organisation:    Example Institute
#> Issuer:          Sondavi, https://sondavi.io
#> Issued:          2026-10-20 08:00 UTC
#> ...
#> Removed columns: respondent_id, ip_address
v$valid
#> [1] TRUE
```

Pass the **files**, not a data frame: reading a CSV into a data frame changes the text
(`"007"` becomes `7`, `"NA"` a missing value), and the certificate is about the text as it
was delivered. The file is read byte for byte.

What the result can say, in the order the checks run:

| status | meaning |
|---|---|
| `malformed` | the certificate file is not a certificate of a supported version |
| `untrusted_issuer` | it comes from an installation you have not listed in `trust` |
| `unknown_key` | the issuer's published keys do not include the one that signed it |
| `invalid_signature` | the certificate itself was altered |
| `unverifiable` | the file does not have the certificate's structure (a column deleted by hand, rows missing, a row with more or fewer fields than the header, not a well-formed CSV) |
| `modified` | the certificate is genuine, but cells differ — `v$modified_cells` names them |
| `valid` | every certified cell matches |

Columns you added yourself are fine — the result lists them (`added_columns`), as it lists
certified columns and rows that were left out (`removed_columns`, `removed_rows`).
Changing a value, inventing or reordering rows is found, and so is deleting columns or
rows by hand, or **opening the file in a spreadsheet program and saving it again**: that
rewrites numbers and dates, and the cells concerned show up as modified. Pass the file on
as it was downloaded.

The file is read strictly, as every verifier of the platform reads it: valid UTF-8, comma,
quotes only around a whole field, line ends LF or CRLF, every row as wide as the header.
Anything another program could read differently makes the check `unverifiable` rather than
let a file pass that R or pandas would read shifted.

The result also says what the certificate itself marks: columns the platform computed from
the questionnaire when it wrote the file ("Computed, not certified" — attention checks,
quality scores, reversed copies), values the participant's browser reported rather than the
platform measured ("Reported by the browser" — times, order, device), and responses collected
on offline devices, whose completion time is the device's.

When cells differ in several rows *and* several columns, `v$suspect` is `TRUE`: the
certificate holds one sum per row and one per column, not each cell, so the cells listed
are where a differing row meets a differing column — suspects, not all of them changed.

**Trust is explicit.** Anyone can run an installation with a key of their own, so only
`https://sondavi.io` is accepted by default. The keys of `https://sondavi.io` known when
this version of the package was released are built into it, and a certificate signed with
one of them is checked without a connection. For a newer key the issuer's published keys
are fetched from `<issuer>/.well-known/sondavi-certificate-keys.json` — and only once the
issuer is trusted. A certificate from another installation reads as `untrusted_issuer`
until you name that installation in `trust` — the `README.txt` that comes with a certified
download gives the address to pass. Its keys are then fetched from there, or you pass them
yourself (then only those are used):

```r
sondavi_verify("data.csv", "data.certificate.json",
               trust = "https://survey.example.org", keys = "keys.json")
```

### A file you had certified

The platform can also certify a file you already have — a CSV, Excel or JSON file of the
responses, perhaps cleaned or reduced: it compares every value with the stored responses,
or with a file it certified earlier, and then signs the file's fingerprint (the SHA-256 of
its bytes) together with what the comparison found. Check it the same way, with the file
itself:

```r
v <- sondavi_verify("wave2-clean.xlsx", "wave2-clean.certificate.json")
v
#> Valid: this is the file that was certified.
#> ...
#> How:             Certified from the researcher's own Excel file, after every value was compared with a file certified earlier
#> Derived from:    certificate 8869174a of 2026-10-20 (wave2.csv)
#> File:            wave2-clean.xlsx (48,213 bytes)
#> Read as:         Excel, sheet "Responses"
#> Rows matched:    by response ID
#> Rows:            312 of 318 responses of this survey
#> Left out:        6 responses: 4 incomplete, 2 screened out
#> Not in the file: respondent_id, ip_address
#> Own columns:     age_group (not checked)
#> Notation:        q_num (12 cells) - the same value in another notation
#> Row order:       changed
v$kind
#> [1] "file"
```

The file is hashed, not read, so **any** change makes it another file — also opening it
and saving it again unchanged; the result is then `modified`. What the comparison found
is in `v$manifest$basis`; `Derived from` names the earlier certificate the file was
compared with, when it was.

### Sharing fewer columns or rows

To share a version with fewer columns or rows — without the pseudonyms, or without the
people who did not agree to sharing — or in another format, certify the changed file on
the platform that issued the certificate ("Certify a file you have", compared with the
certified file). That works even after the responses were deleted there, and the new
certificate says what was left out. Deleting columns or rows by hand, or saving the file
again from a spreadsheet program, makes the certificate fail.

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

The certificate checks need no server and run on their own as well:

```
Rscript tests/certificate-tests.R
```

They use files the platform itself issued (`tests/fixtures/certificates/`) — a valid
dataset, one with a column and a row left out, a tampered one, certified files — the same
set the platform's own verifier is checked against.

## License

MIT
