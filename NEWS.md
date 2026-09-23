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
