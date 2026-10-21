# sondavi 0.9.0

## Scale scores of the questionnaire, worked out again

* `sondavi_scale(flat, score = "sdq_emo")` works out a scale score the questionnaire computes
  itself (Σ in the editor): the codebook now says which rows it counts - those of its
  subscales, in a loop those of the same copy -, mean or sum, and how many have to be
  answered. That is the way to give responses recorded before a score was added or changed
  the score of now; the platform keeps every response's score as it was computed then.
  Needs a platform that describes its scale scores (version 0.106 or later); with an older
  one, name the items.
* `sondavi_scale()` takes `method = "sum"` as well as the mean.
* `sondavi_codebook()` has two new columns: `subscale` (a matrix row's subscale) and
  `scale_score` (a list column: how a scale score is worked out).

## Questions with several answers

* `sondavi_options()` gives one column per option of a multiple-choice question, a tag box,
  a picture choice or a ranking, as the export files have them: 1 when the option was
  chosen, 0 when the question was answered without it, `NA` when it was not answered; for a
  ranking the option's place.
* `sondavi_unnest()` keeps such an answer as one list column and adds those option columns
  right after it (`options = FALSE` leaves them out). It used to spread the list by
  position - `devices.0`, `devices.1` - which read like options and were only "the first
  one chosen", "the second one chosen".

## Tested against more of what researchers ask

The tests now include a study with multiple choice ("None", "Other" with its text), a tag
box, a ranking, a rating, yes/no, a drop-down with "Don't know", text boxes, a constant sum
with its total, an SDQ-like matrix with subscales and the questionnaire's own scale scores,
and a loop with a score per copy - recorded from the platform, and checked value by value
against the platform's own export file. The platform's scale score cases now include sums.

## The files participants uploaded

`sondavi_files(con, survey_id)` downloads the files of a study's "File Upload" questions and
unpacks them: a folder per question, the files named by response (`drawing/1234-1.png`) rather
than by the name a participant gave them, and a data frame with one row per file - whose it
is (`response_id`, and `respondent_id` where your token may read it), the original name, type,
size, checksum and `local_path`, where the file now is. Join it to `sondavi_responses()` over
`response_id`.

In the responses themselves an uploaded file now appears as its name, type, size and a
reference; the file is no longer in the row.

# sondavi 0.8.1

* A check in the package's own tests passed a text where R expects `TRUE`. R 4.6 refuses
  that, so the tests failed there. The package itself is unchanged.

# sondavi 0.8.0

## Certified datasets

The platform can now issue a dataset with a certificate: the CSV file plus a
`<name>.certificate.json` it signs, vouching that every cell holds exactly what the
participants entered.

* `sondavi_verify(data, certificate)` checks such a pair on your own computer and says
  whether every certified cell matches - and if not, which cells differ, or why the file
  cannot be checked at all. It also names the study, the organisation, the issuer and the
  date, says how many rows were left out, and sorts the columns of the file as the platform's
  verify page does, each in one place: certified, in the file but not certified (left out when
  the certificate was issued, computed by the platform, or added afterwards) and missing.
* It takes the **files**, not a data frame: a data frame has already turned `"007"` into
  `7`. For the same reason a file opened in a spreadsheet program and saved again is
  reported as modified.
* It also checks the certificate of a file you had certified yourself - a CSV, Excel or JSON
  file the platform compared with the stored responses, or with a file it certified earlier.
  Such a file is hashed, not read: it is valid when its bytes are the ones certified, and the
  result lists what the comparison found (responses left out, by status and group; columns
  not in the file and your own; values in another notation; the row order) and, for a file
  compared with one certified earlier, which certificate that was ("Derived from").
  `v$kind` says which kind of certificate it is.
* To share a version with fewer columns or rows, or in another format, certify the changed
  file on the platform that issued the certificate ("Certify a file you have", compared with
  the certified file) - that works even after the responses were deleted there. Deleting
  columns or rows by hand, or saving the file again from a spreadsheet program, makes the
  certificate fail.
* The data file is read strictly, as every verifier of the platform reads it: valid UTF-8,
  quotes only around a whole field, line ends LF or CRLF, and every row exactly as wide as
  the header. A file another program could read differently - a row longer than the header,
  which R and pandas read shifted, for one - is reported as unverifiable.
* The summary names what the certificate marks: columns computed from the questionnaire
  rather than certified, values reported by the participant's browser, and responses from
  offline devices; for a certified file also how it was read and how its rows were matched.
* Only `https://sondavi.io` is trusted by default; a certificate from another installation
  is reported as coming from an untrusted issuer until you list it in `trust`.
* The keys of `https://sondavi.io` known when this version was released are built into the
  package, so a certificate signed with one of them is checked without a connection. A newer
  key is fetched from the issuer; keys passed with `keys =` are used instead of both.

The package now imports `openssl` (already installed wherever `httr2` is). The checks run
against files the platform itself issued, so this package, the platform and the browser
check give the same answer for the same pair.

# sondavi 0.7.1

## Scale scores as the questionnaire computes them

`sondavi_scale()` now gives the same numbers as the scale score the platform computes in the
questionnaire ("Add scale score"). Two cases differed:

* An item marked "Reverse-scored" whose scale has no numbers (columns named `column1`,
  `column2` ...) was counted as answered, unmirrored. It now counts as unanswered, with a
  warning naming it - its reversed copy is empty in the platform's export, too.
  `sondavi_items()` gives it as `NA`.
* A `min_items` above the number of items gave `NA` for every row. It now means every item.

The package checks itself against the platform's own cases, so the two cannot drift apart
unnoticed.

# sondavi 0.7.0

## Every row says what became of it

* Each response now carries `response_status`: `complete`, `partial` (stopped before the end)
  or `screened_out`. It arrives as a factor with all three levels, in this order, so
  `table(d$response_status)` shows a 0 where nobody was screened out instead of leaving the
  category out, and `subset(d, response_status == "complete")` keeps the completed ones.
  `sondavi_snapshot_responses()` does the same.
* A status this version does not know is kept as a level of its own, with a warning, rather
  than becoming `NA`.

`subset(d, !is.na(completed_at))` still works and gives the same rows.

# sondavi 0.6.1

## Screened-out participants and bot checks

* A study can now screen people out (the platform's "Screen out participant" rule). Such a
  person is a row like an unfinished one - `completed_at` is `NA` - and carries
  `screened_out_at` and `screen_out_reason` instead. `screened_out_at` arrives as a
  `POSIXct` timestamp like `completed_at`. Counting finished participations with
  `subset(d, !is.na(completed_at))` therefore leaves screen-outs out, as it should.
* A bot check asked as a question arrives as a column of its own, `"Passed"` / `"Failed"`
  with its labels, and `bot_check_passed` is filled for every study that checks.

The tests now include a study with a screen-out and a bot check question, recorded from a
real instance.

# sondavi 0.6.0

## AI interviews

* `sondavi_unnest()` writes an AI interview question the way the platform's export and codebook do:
  `<question>.status`, `.turns`, and where they apply `.answered`, `.skipped` and `.summary`.
  It used to spread the stored conversation into one column per message and field
  (`talk.messages.3.text` …), and the columns the codebook names for the question were missing.
* `sondavi_conversations(d)` is the conversation as a long table, one row per message: who wrote it, the
  respondent's answer number, the text, typed or spoken, and when.

The tests now include a study with AI interviews, recorded from a real run.

# sondavi 0.5.0

## Scale scores

* `sondavi_scale(flat, items)` is the scale score per row, `sondavi_items(flat, items)`
  the items as numbers (for `cor()`, `lm()`, `psych::alpha()`). Name the items as in the
  questionnaire: a reverse-scored one is taken from its reversed copy by itself.
* They read the numbers from the codebook. `as.numeric()` on the factors
  `sondavi_responses()` returns gives the position of the level: a "Don't know" offered
  as sixth option became 6.
* By default every item has to be answered; `min_items` allows gaps on purpose.

The tests now also compare the reversed copies with the platform's own export file of the
same study, value by value.

# sondavi 0.4.0

## Reverse-scored items

On the platform you can mark a matrix row as "Reverse-scored": an item worded the
other way round within a scale. The answer is stored as given, and the platform's
export files add a reversed copy `<name>_r` = (smallest + largest scale value) -
answer.

* `sondavi_unnest()` now adds the same column, right after the item it mirrors: a
  number, computed from the answer as given, and `NA` for an answer off the scale
  such as "don't know". Name and arithmetic are the platform's, so `b5.worry_r`
  means the same in an exported file and in data read through the API.
* `sondavi_codebook()` has three new columns: `reverse_scored`, and `reverse_min`
  and `reverse_max`, the range the answers are mirrored in (`NA` when the row's
  scale has no numbers).

This needs a platform that sends `reverse_scored` in the codebook; with an older one
there is simply no copy.

## Matrix cells get their labels

`sondavi_unnest()` now gives the columns it creates the codebook's treatment, as
`sondavi_responses()` does for the questions at the top level: a matrix cell with
value labels becomes a factor, a numeric one a number, and each carries its
question text. Before, a matrix cell stayed a bare code next to a question that is a
factor.

For this, `sondavi_responses()` and `sondavi_snapshot_responses()` keep the codebook
with the data, as the attribute `sondavi_codebook`. With `labels = FALSE` there is
none and the cells stay as stored, as they do with
`sondavi_unnest(d, codebook = NULL)`.

## Answers without a label are kept (changes existing behaviour)

A value the codebook has no label for - an answer to an option deleted after people
had chosen it, a code added later without a label - used to become `NA` when its
column became a factor. `NA` reads as "not answered", so the answer was lost without
notice. It is now kept as a level of its own, named by the value, after the labelled
levels, and a warning names the column and the values.

A numeric variable holding something that is no number still has `NA` there, but now
with a warning saying what was dropped.

## Fixed

* Yes/no questions arrived as factors that were entirely `NA`: the codebook writes
  their values as `true`/`false`, and the answers were compared as `TRUE`/`FALSE`.
* A numeric code of six digits or more (100000) did not find its label: it was
  compared as `"1e+05"`.

## On the platform's side

The rows now also carry the platform's internal fields under their own names
(`__locale`, `__randomization_order`, ...), and a question named like one of the
row's own fields (`source`, `locale`, `completed_at`, ...) arrives as
`<name>_answer`. The package passes both through unchanged. **Server-side change —
your platform needs updating for it.**

# sondavi 0.3.0

## Image marking questions

The platform has a new question type: participants paint areas or set pins on an
image. Its answer arrives as a list column holding the image, the grid and the cells
or pins.

* `sondavi_markings()` turns these into one row per painted cell or pin, with the
  position relative to the image and in pixels of the original - the same table as
  `markings.csv` in the platform's image-markings export. This is what a heatmap is
  drawn from.
* `sondavi_unnest()` writes them the way the CSV export does: one column per marking
  type, cells as row runs (`"2:3-5 3:4"`), pins as `"x,y"` pairs. Before, it spread
  the stored object into `map.image.src`, `map.grid.cols`, ... - column names the
  codebook does not know.

# sondavi 0.2.1

Test infrastructure only; the package itself is unchanged.

The run picks a free port instead of a fixed one, and checks that the answer on it comes
from the fixture server rather than merely that something is listening — a foreign
service passed the old check, and the tests then ran against it and hung. When the server
does not come up, its output is printed instead of a bare timeout.

Not a theoretical worry: the Python package lost a CI cycle to a fixed port a macOS
runner already held.

# sondavi 0.2.0

## Timestamps are timestamps (changes existing behaviour)

`completed_at` and `started_at` now arrive as `POSIXct` in the platform's own
time zone, which the API states in its response. They used to be character
columns.

This was worth a breaking change because the old behaviour failed **silently**.
Comparing a character column against a date returns an answer rather than an
error, and sorting by one sorts lexically:

```r
subset(d, completed_at > Sys.time() - 7 * 86400)   # plausible, and not a date comparison
```

If your script worked around this with `as.POSIXct()`, that call is now
redundant but harmless.

## New

* `sondavi_unnest()` spreads matrices and dynamic panels into one column each,
  using exactly the names the platform's own export writes — so an analysis
  built on an exported file and one built on the API agree on variable names.
* `sondavi_waves()` joins the waves of a study series on the respondent
  identifier. Everyone seen in any wave is kept, so the people who stopped
  answering stay in the table instead of being dropped by an inner join.
* A vignette, `vignette("sondavi")`: one analysis end to end.
* Every exported function now has examples.

## Fixed

* Replaying a snapshot with a token that reads fewer columns than it was
  recorded with no longer warns that the dataset has changed. The digest covers
  the rows as delivered, so a narrower replay differs by design; you now get a
  message saying the recorded fingerprint does not apply, and the completeness
  of the set is reported separately. A warning that fires when nothing is wrong
  teaches you to ignore the one case it exists for.
* Device traits (browser, operating system, screen resolution, window size, time
  zone, browser language) are now covered by the token's technical ability. They
  reach the row as `meta_*` columns and previously came through regardless of
  what the token was granted. **Server-side fix — your platform needs updating
  for it to take effect.**

## Documentation

* The help pages render properly (Markdown was not enabled, so they showed raw
  backticks and `[links()]`).
* `sondavi_responses()` documents that **partial responses are included** when
  the study saves them: `nrow()` is not the number of completed participations.

# sondavi 0.1.0

First release: `sondavi_connect()`, `sondavi_surveys()`, `sondavi_codebook()`,
`sondavi_responses()`, `sondavi_fingerprint()`, and snapshots for citable
datasets.
