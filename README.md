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

`sondavi_responses()` applies the codebook by default, so a categorical question arrives as
a factor with its real labels rather than as bare codes, and the question text rides along
as a `label` attribute. Pass `labels = FALSE` for the raw values.

Nested answers — matrices, dynamic panels, jsPsych trials — stay as list columns. Flattening
them here would invent a shape the platform did not give.

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

Answers always come through. The participant identifier and the IP address only if the token
was given those abilities when you created it, and never beyond what the study's own privacy
level allows — an anonymous study returns no identifier whatever the token says.

## Tests

```
tests/run.sh
```

Starts a small server that replays **real answers captured from the platform**
(`tests/fixtures/`), including one recorded after a response was erased. No network, no
credentials. Under `R CMD check` these are skipped, since they need that server.

## License

MIT
