# Certified datasets: plain-R checks (testthat is not installed on the build host).
#   Rscript tests/certificate-tests.R
# Offline: no fixture server, no network. The files in tests/fixtures/certificates/ were
# written by the platform itself - the same set the platform's own verifier and the Python
# package are checked against, so the three cannot drift apart unnoticed.

# Installed package if there is one (that is how `R CMD check` runs), otherwise the
# sources next door (that is how one runs it while working on them).
INSTALLED <- requireNamespace("sondavi", quietly = TRUE)
pkg_root <- if (dir.exists("R")) "." else ".."
if (!INSTALLED) {
  for (f in list.files(file.path(pkg_root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f)
} else {
  library(sondavi)
  # The internals the checks below reach into.
  read_csv_table <- getFromNamespace("read_csv_table", "sondavi")
  keys_by_id <- getFromNamespace("keys_by_id", "sondavi")
  builtin_key_document <- getFromNamespace("builtin_key_document", "sondavi")
  # Not offered any more (the platform certifies a changed file again), but the reference for the
  # offline way the published format allows - so it is still checked here.
  sondavi_redact <- getFromNamespace("sondavi_redact", "sondavi")
}

# Replaces internal functions for the length of `code` - in the package's namespace when it is
# installed (that is where its functions look them up), otherwise where the sources were read.
with_replaced <- function(replacements, code) {
  originals <- lapply(names(replacements), function(name) {
    if (INSTALLED) getFromNamespace(name, "sondavi") else get(name, envir = globalenv())
  })
  names(originals) <- names(replacements)
  put <- function(values) for (name in names(values)) {
    if (INSTALLED) utils::assignInNamespace(name, values[[name]], "sondavi") else assign(name, values[[name]], envir = globalenv())
  }
  put(replacements)
  on.exit(put(originals))
  force(code)
}

FX <- if (dir.exists("fixtures/certificates")) "fixtures/certificates" else "tests/fixtures/certificates"
fx <- function(name) file.path(FX, name)
TRUST <- "https://certificates.example"
KEYS <- fx("keys.json")
expected <- jsonlite::fromJSON(fx("expected.json"), simplifyVector = FALSE)

failures <- 0L
checks <- 0L

ok <- function(label, expr) {
  checks <<- checks + 1L
  result <- tryCatch(isTRUE(expr), error = function(e) {
    cat("   error:", conditionMessage(e), "\n"); FALSE
  })
  cat(if (result) "  \u2713 " else "  \u2717 ", label, "\n", sep = "")
  if (!result) failures <<- failures + 1L
}

fails_with <- function(label, pattern, expr) {
  checks <<- checks + 1L
  msg <- tryCatch({ force(expr); NA_character_ }, error = conditionMessage)
  result <- !is.na(msg) && grepl(pattern, msg, fixed = TRUE)
  cat(if (result) "  \u2713 " else "  \u2717 ", label, "\n", sep = "")
  if (!result) { failures <<- failures + 1L; cat("   got:", msg, "\n") }
}

verify <- function(data, certificate = fx("dataset.certificate.json"), trust = TRUST, keys = KEYS) {
  sondavi_verify(data, certificate, trust = trust, keys = keys)
}

# Small file helpers: everything is bytes here, never re-encoded.
bytes_of <- function(path) readBin(path, "raw", file.size(path))
text_of <- function(path) { x <- rawToChar(bytes_of(path)); Encoding(x) <- "UTF-8"; x }
write_text <- function(path, text) writeBin(charToRaw(enc2utf8(text)), path)
tmp <- function(ext) tempfile(fileext = ext)
dataset_text <- text_of(fx("dataset.csv"))

# A certificate with something changed, written next to the test files.
edited_certificate <- function(edit) {
  cert <- jsonlite::fromJSON(fx("dataset.certificate.json"), simplifyVector = FALSE)
  cert <- edit(cert)
  path <- tmp(".certificate.json")
  write_text(path, as.character(jsonlite::toJSON(cert, auto_unbox = TRUE, null = "null", digits = NA)))
  path
}

cat("\nReading the CSV exactly\n")
# The cells as the platform wrote them (tests/Support/CertificateFixtures.php, ip_address
# left out at issue): every one of them is a trap for some CSV reader.
HEADER <- c("response_id", "response_status", "respondent_id", "q_text", "q_num", "q_multi",
            "Spalte mit \u00dcmlaut", "comma,name")
ROWS <- rbind(
  c("1", "complete", "P-001", "He said \"hi\", then left", "007", "[1,2]", "Gr\u00fc\u00dfe \U0001F30D", "x"),
  c("2", "partial", "P-002", "line1\nline2", "3.10", "", "NA", "=HYPERLINK(\"http://x\",\"y\")"),
  c("3", "complete", "P-003", "CRLF\r\ninside", "-0", "a\\\"b", " leading and trailing ", ""),
  c("4", "screened_out", "P-004", "", "1e5", "semi;colon", "\t tab", "\"")
)
tab <- read_csv_table(fx("dataset.csv"))
ok("the header is read as written, an umlaut and a comma in names included", identical(tab$header, HEADER))
ok("every cell is the exact text the platform wrote", identical(unname(tab$cells), ROWS))
ok("a line break inside quotes stays \\r\\n, as written", tab$cells[3, 4] == "CRLF\r\ninside")
ok("\"NA\" stays text, \"007\" and \"3.10\" stay as written", tab$cells[2, 7] == "NA" && tab$cells[1, 5] == "007" && tab$cells[2, 5] == "3.10")
ok("spaces and tabs around a value are kept", tab$cells[3, 7] == " leading and trailing " && tab$cells[4, 7] == "\t tab")
ok("the cells are UTF-8", all(validUTF8(tab$cells)) && Encoding(tab$cells[1, 7]) == "UTF-8")

crlf <- tmp(".csv")
# Line ends of the rows only - the line breaks inside quoted cells stay what they were.
write_text(crlf, "\ufeffa,b\r\n\"x\r\ny\",\"p\nq\"\r\n1,2")
t2 <- read_csv_table(crlf)
ok("CRLF between rows, a last row without a line end, breaks inside quotes kept",
   identical(t2$header, c("a", "b")) && identical(unname(t2$cells), rbind(c("x\r\ny", "p\nq"), c("1", "2"))))
one <- tmp(".csv")
write_text(one, "a\n\nx\n\n")
ok("in a one-column file an empty line is an empty cell", identical(as.vector(read_csv_table(one)$cells), c("", "x", "")))
blank <- tmp(".csv")
write_text(blank, "a,b\n\n1,2\n\n")
ok("in a wider file an empty line is no row", identical(unname(read_csv_table(blank)$cells), rbind(c("1", "2"))))
short <- tmp(".csv")
write_text(short, "a,b,c\n1,\n4,5,6,7\n")
ok("rows are kept as read, a short and a long one included (the check refuses both)",
   identical(read_csv_table(short)$rows, list(c("1", ""), c("4", "5", "6", "7"))) &&
     identical(read_csv_table(short)$widths, c(2L, 4L)))

# The cases every verifier reads alike (the platform's, the browser's, Python's): the bytes, and
# the header and rows they must give - or "invalid".
csv_cases <- jsonlite::fromJSON(fx("csv-cases.json"), simplifyVector = FALSE)$cases
ok("the shared CSV cases are all there", length(csv_cases) >= 24)
for (case in csv_cases) {
  path <- tmp(".csv")
  writeBin(openssl::base64_decode(case$base64), path)
  got <- tryCatch({
    t <- read_csv_table(path)
    list(t$header, t$rows)
  }, sondavi_csv_error = function(e) "invalid", error = function(e) paste("error:", conditionMessage(e)))
  want <- if (identical(case$result, "invalid")) "invalid" else
    list(as.character(unlist(case$result[[1]])), lapply(case$result[[2]], function(r) as.character(unlist(r))))
  ok(paste("CSV case:", case$name), identical(got, want))
}
open_quote <- tmp(".csv")
write_text(open_quote, "a,b\n1,\"never closed\n")
ok("a quote that never closes is no readable CSV",
   inherits(tryCatch(read_csv_table(open_quote), error = identity), "sondavi_csv_error"))

# Inside the platform's repository the originals sit next door (four levels up from tests/);
# in the published package they do not, and the comparison is skipped.
platform_fixtures <- file.path(pkg_root, "..", "..", "..", "tests", "fixtures", "certificates")
if (dir.exists(platform_fixtures)) {
  originals <- sort(list.files(platform_fixtures))
  ok("every file of the platform's fixture set is here, byte for byte",
     all(vapply(originals, function(n) file.exists(fx(n)) && identical(bytes_of(fx(n)), bytes_of(file.path(platform_fixtures, n))), NA)))
  ok("and nothing else is", identical(sort(list.files(FX)), originals))
  ok("the built-in keys are the platform's copy, byte for byte",
     identical(bytes_of(file.path(pkg_root, "inst", "certificate-keys.json")), bytes_of(file.path(pkg_root, "..", "..", "certificate-keys.json"))))
} else {
  cat("  (the platform's own fixture files are not next door: copies not compared)\n")
}

cat("\nThe platform's own files check as it says they do\n")
v <- verify(fx("dataset.csv"))
ok("dataset: valid", v$status == expected$dataset$status && isTRUE(v$valid))
ok("dataset: ip_address was left out at issue", identical(v$removed_columns, unlist(expected$dataset$removed_columns)))
ok("dataset: no rows removed, no columns added", v$removed_rows == 0 && length(v$added_columns) == 0)
ok("it is a sondavi_verification", inherits(v, "sondavi_verification"))
ok("it names the certificate, organisation, study, issuer and date",
   v$certificate == "00000000-0000-4000-8000-000000000086" && v$organization == "Test Organisation" &&
     v$survey$title == "TEST Certified" && v$issuer$url == TRUST &&
     identical(v$issued_at, as.POSIXct("2026-10-20 08:00:00", tz = "UTC")))
shown <- capture.output(print(v))
ok("printed, it says so in one line first", shown[1] == "Valid: every certified cell matches.")
ok("and says the column left out at issue is missing from the file",
   any(shown == "Missing:         1 column") &&
     any(shown == paste0(strrep(" ", 17), "left out when the certificate was issued: ip_address")) &&
     any(shown == "Not certified:   none") &&
     any(shown == "Certified:       8 columns, every value as participants entered it"))

r <- verify(fx("redacted.csv"), fx("redacted.certificate.json"))
ok("redacted: valid", r$status == expected$redacted$status)
ok("redacted: respondent_id and ip_address removed", identical(r$removed_columns, unlist(expected$redacted$removed_columns)))
ok("redacted: one row removed", r$removed_rows == expected$redacted$removed_rows)
ok("printed: the removed row is counted", any(grepl("Removed rows: +1", capture.output(print(r)))))

m <- verify(fx("tampered.csv"))
ok("tampered: modified", m$status == expected$tampered$status && !m$valid)
ok("exactly the changed cell is named: row 3, q_num",
   identical(m$modified_cells, data.frame(row = 3L, column = "q_num", stringsAsFactors = FALSE)))
ok("one cell is no suspect, and there is nothing else to report", !m$suspect && length(m$problems) == 0)
shown <- capture.output(print(m))
ok("printed, it names the cell", grepl("^Modified", shown[1]) && any(grepl("row 3, column q_num", shown, fixed = TRUE)))

cat("\nThe keys can be given in every form\n")
ok("parsed as lists", verify(fx("dataset.csv"), keys = jsonlite::fromJSON(KEYS, simplifyVector = FALSE))$valid)
ok("parsed the jsonlite default way (a data frame of keys)", verify(fx("dataset.csv"), keys = jsonlite::fromJSON(KEYS))$valid)
ok("a trailing slash in the trusted address is harmless", verify(fx("dataset.csv"), trust = paste0(TRUST, "/"))$valid)
other_key <- openssl::ed25519_keygen()
other_pub <- as.list(other_key$pubkey)$data
id_of <- function(pub) paste0("ed25519:", substr(paste(as.character(unclass(openssl::sha256(pub))), collapse = ""), 1, 16))
key_entry <- function(id, pub) list(id = id, algorithm = "Ed25519", public_key = openssl::base64_encode(pub), status = "active")
fixture_keys <- jsonlite::fromJSON(KEYS, simplifyVector = FALSE)
ok("the fixture key's id is the one this package computes from it",
   id_of(openssl::base64_decode(fixture_keys$keys[[1]]$public_key)) == fixture_keys$keys[[1]]$id)
both <- list(keys = list(key_entry(id_of(other_pub), other_pub), fixture_keys$keys[[1]]))
ok("an issuer with several keys: the one that signed is found", verify(fx("dataset.csv"), keys = both)$valid)
ok("keys of the issuer that do not include the signing key: unknown_key",
   verify(fx("dataset.csv"), keys = list(keys = list(key_entry(id_of(other_pub), other_pub))))$status == "unknown_key")
# Another key filed under the signing key's id: taken, it would turn every certificate into
# "invalid_signature"; a key whose id is not its own is no key at all.
impostor <- list(keys = list(key_entry(fixture_keys$keys[[1]]$id, other_pub)))
ok("a key under an id that is not its own is ignored: unknown_key",
   verify(fx("dataset.csv"), keys = impostor)$status == "unknown_key")
ok("a keys document without keys: unknown_key", verify(fx("dataset.csv"), keys = list(keys = list()))$status == "unknown_key")
fails_with("a file that is no keys document is refused", "not a keys document",
           verify(fx("dataset.csv"), keys = fx("dataset.certificate.json")))

cat("\nTrust is explicit\n")
u <- verify(fx("dataset.csv"), trust = "https://sondavi.io")
ok("an issuer that is not trusted: untrusted_issuer", u$status == "untrusted_issuer" && !u$valid)
ok("and it says whom the certificate names", any(grepl(TRUST, capture.output(print(u)), fixed = TRUE)))
# No keys given, default trust: the issuer is refused before anything would be fetched -
# this runs offline because the package never contacts an issuer it does not trust.
ok("by default only sondavi.io is trusted, and nothing is fetched from anyone else",
   sondavi_verify(fx("dataset.csv"), fx("dataset.certificate.json"))$status == "untrusted_issuer")

cat("\nWhat is found\n")
edited <- edited_certificate(function(cert) {
  cert$manifest <- sub("Test Organisation", "Another Organisation", cert$manifest, fixed = TRUE)
  cert
})
ok("an edited manifest: invalid_signature", verify(fx("dataset.csv"), edited)$status == "invalid_signature")
bad_sig <- edited_certificate(function(cert) { cert$signature <- "not base64!"; cert })
ok("a signature that is no base64: invalid_signature", verify(fx("dataset.csv"), bad_sig)$status == "invalid_signature")
short_sig <- edited_certificate(function(cert) { cert$signature <- substr(cert$signature, 1, 40); cert })
ok("a cut signature: invalid_signature", verify(fx("dataset.csv"), short_sig)$status == "invalid_signature")

no_column <- tmp(".csv")
d <- read_csv_table(fx("dataset.csv"))
write_csv_table <- if (exists("write_csv_table")) write_csv_table else getFromNamespace("write_csv_table", "sondavi")
write_csv_table(no_column, d$header[-6], d$cells[, -6, drop = FALSE])
gone <- verify(no_column)
ok("a column deleted by hand: unverifiable", gone$status == "unverifiable")
ok("naming the column and the way to share fewer columns, in the platform's words",
   identical(gone$problems, "The certified column \"q_multi\" is missing from the file. To share fewer columns, certify the changed file again where the certificate was issued."))
no_row <- tmp(".csv")
write_csv_table(no_row, d$header, d$cells[-2, , drop = FALSE])
ok("a row deleted by hand: unverifiable, with the counts",
   identical(verify(no_row)$problems, "The file has 3 data rows, the certificate accounts for 4. To share fewer rows, certify the changed file again where the certificate was issued."))
swapped <- tmp(".csv")
write_csv_table(swapped, d$header, d$cells[c(2, 1, 3, 4), , drop = FALSE])
sw <- verify(swapped)
ok("two rows swapped: modified, and the cells are suspects", sw$status == "modified" && sw$suspect && all(sw$modified_cells$row %in% 1:2))
two <- d$cells
two[1, 5] <- "7"
two[4, 2] <- "complete"
twice <- tmp(".csv")
write_csv_table(twice, d$header, two)
tw <- verify(twice)
ok("two cells in different rows and columns: the four crossings are suspects",
   tw$suspect && identical(tw$modified_cells,
     data.frame(row = c(1L, 1L, 4L, 4L), column = c("q_num", "response_status", "q_num", "response_status"), stringsAsFactors = FALSE)))
ok("printed, it says they are suspects", any(grepl("suspects", capture.output(print(tw)))))
dup <- tmp(".csv")
write_csv_table(dup, c(d$header, "q_num"), cbind(d$cells, "x"))
ok("a duplicate column name: unverifiable", identical(verify(dup)$problems, "The data file has duplicate column names."))
tl <- verify(fx("too_long.csv"))
ok("a data row longer than the header: unverifiable, in the platform's words",
   tl$status == expected$too_long$status && identical(tl$problems, unlist(expected$too_long$problems)))
short_row <- tmp(".csv")
write_text(short_row, sub(",x\n", "\n", dataset_text, fixed = TRUE))
ok("and one shorter than the header too",
   identical(verify(short_row)$problems, "Data row 1 has 7 fields, the header 8."))
ok("a CSV that cannot be read: unverifiable, in the platform's words",
   identical(verify(open_quote)$problems, "The data file is not a well-formed CSV file."))
leaf_changed <- edited_certificate(function(cert) {
  cert$proof$columns[[7]]$leaves[[2]] <- strrep("0", 64)
  cert
})
lc <- verify(fx("dataset.csv"), leaf_changed)
ok("a changed record of a removed column: modified, no cell to blame",
   lc$status == "modified" && nrow(lc$modified_cells) == 0 &&
     identical(lc$problems, "The certificate's record of removed data was altered."))
rows_changed <- edited_certificate(function(cert) { cert$proof$rows <- list(0L, 1L, 2L, 2L); cert })
ok("a row list that names a row twice: unverifiable",
   identical(verify(fx("dataset.csv"), rows_changed)$problems, "The certificate's row list is invalid."))
unreadable <- "The certificate's record could not be read."
# An invented row hiding behind a row number used twice: without the duplicate check its
# cells would never be compared with anything.
invented <- tmp(".csv")
red <- read_csv_table(fx("redacted.csv"))
write_csv_table(invented, red$header, rbind(c("99", "complete", "invented", "1", "", "x", "y"), red$cells))
edited_redacted <- function(edit) {
  cert <- jsonlite::fromJSON(fx("redacted.certificate.json"), simplifyVector = FALSE)
  path <- tmp(".certificate.json")
  write_text(path, as.character(jsonlite::toJSON(edit(cert), auto_unbox = TRUE, null = "null", digits = NA)))
  path
}
twice_zero <- edited_redacted(function(cert) { cert$proof$rows <- list(0L, 0L, 2L, 3L); cert })
iv <- verify(invented, twice_zero)
ok("an invented row behind a row number used twice: unverifiable, the row list is invalid",
   iv$status == "unverifiable" && identical(iv$problems, "The certificate's row list is invalid."))
ok("as is a row number outside the certificate or no whole number",
   identical(verify(fx("dataset.csv"), edited_certificate(function(cert) { cert$proof$rows[[4]] <- 4L; cert }))$problems,
             "The certificate's row list is invalid.") &&
   identical(verify(fx("dataset.csv"), edited_certificate(function(cert) { cert$proof$rows[[4]] <- 2.5; cert }))$problems,
             "The certificate's row list is invalid."))
garbage_seed <- edited_certificate(function(cert) { cert$proof$columns[[1]]$seed <- "not hex at all"; cert })
gs <- verify(fx("dataset.csv"), garbage_seed)
ok("a seed that is no hex at all: unverifiable, the record cannot be read",
   gs$status == "unverifiable" && identical(gs$problems, unreadable))
garbage_leaf <- edited_certificate(function(cert) { cert$proof$columns[[7]]$leaves[[1]] <- "zz"; cert })
ok("so is a leaf that is no hex", identical(verify(fx("dataset.csv"), garbage_leaf)$problems, unreadable))
short_seed <- edited_certificate(function(cert) {
  cert$proof$columns[[1]]$seed <- substr(cert$proof$columns[[1]]$seed, 1, 32)
  cert
})
ok("a seed that is not 32 bytes: the record cannot be read",
   identical(verify(fx("dataset.csv"), short_seed)$problems, unreadable))
short_salt <- edited_redacted(function(cert) {
  cert$proof$columns[[1]]$salts[[1]] <- substr(cert$proof$columns[[1]]$salts[[1]], 1, 30)
  cert
})
ok("a salt that is not 16 bytes: the record cannot be read",
   identical(verify(fx("redacted.csv"), short_salt)$problems, unreadable))
salt_for_removed <- edited_redacted(function(cert) {
  cert$proof$columns[[1]]$salts[[2]] <- strrep("ab", 16)
  cert
})
ok("a salt where the row was removed: inconsistent, not unreadable",
   identical(verify(fx("redacted.csv"), salt_for_removed)$problems,
             "The record of the column \"response_id\" is inconsistent with the rows in the file."))
leaf_for_present <- edited_redacted(function(cert) {
  cert$proof$columns[[1]]$salts[[1]] <- paste0("-", strrep("0", 64))
  cert
})
ok("only a leaf for a row that is in the file: inconsistent (its value would go unchecked)",
   verify(fx("redacted.csv"), leaf_for_present)$status == "unverifiable")
salt_as_list <- edited_redacted(function(cert) {
  cert$proof$columns[[1]]$salts[[2]] <- list("a", "b")
  cert
})
ok("a salt that is a list: the record cannot be read",
   identical(verify(fx("redacted.csv"), salt_as_list)$problems, unreadable))

# What the issuer signed cannot be edited without a key: these manifests are re-signed with a
# throwaway one, whose keys document is passed along.
resigned <- function(edit, source = fx("dataset.certificate.json")) {
  cert <- jsonlite::fromJSON(source, simplifyVector = FALSE)
  manifest <- jsonlite::fromJSON(cert$manifest, simplifyVector = FALSE)
  manifest$key_id <- id_of(other_pub)
  cert$manifest <- as.character(jsonlite::toJSON(edit(manifest), auto_unbox = TRUE, null = "null", digits = NA))
  cert$signature <- openssl::base64_encode(openssl::ed25519_sign(charToRaw(enc2utf8(cert$manifest)), other_key))
  path <- tmp(".certificate.json")
  write_text(path, as.character(jsonlite::toJSON(cert, auto_unbox = TRUE, null = "null", digits = NA)))
  path
}
own_keys <- list(keys = list(key_entry(id_of(other_pub), other_pub)))
ok("re-signed unchanged, the throwaway key checks as valid (the helper itself works)",
   verify(fx("dataset.csv"), resigned(identity), keys = own_keys)$valid)
no_root <- resigned(function(m) { m$columns[[2]]$root <- NULL; m })
ok("a signed manifest whose column lacks its sum: the record cannot be read",
   identical(verify(fx("dataset.csv"), no_root, keys = own_keys)$problems, unreadable))
no_columns <- resigned(function(m) { m$columns <- NULL; m })
ok("a signed manifest without columns: the record cannot be read",
   identical(verify(fx("dataset.csv"), no_columns, keys = own_keys)$problems, unreadable))

not_json <- tmp(".json")
write_text(not_json, "this is not JSON")
ok("a certificate that is no JSON: malformed", verify(fx("dataset.csv"), not_json)$status == "malformed")
version_2 <- edited_certificate(function(cert) { cert$version <- 2L; cert })
ok("a later format version: malformed, with the platform's message",
   identical(verify(fx("dataset.csv"), version_2)$problems, "This is not a certificate of a supported version."))

added <- tmp(".csv")
write_csv_table(added, c(d$header, "my_score"), cbind(d$cells, c("1.5", "2", "", "4")))
ad <- verify(added)
ok("a column of one's own: still valid, listed as added", ad$valid && identical(ad$added_columns, "my_score"))
ok("printed as not certified: added after the certificate was issued",
   any(capture.output(print(ad)) == paste0(strrep(" ", 17), "added after the certificate was issued: my_score")))
# A column the certificate lists as left out, put back with values of one's own: those values
# are new, so the column counts as added - and as removed from what is certified.
back <- tmp(".csv")
write_csv_table(back, c(d$header, "ip_address"), cbind(d$cells, c("10.9.9.1", "10.9.9.2", "", "10.9.9.4")))
bk <- verify(back)
ok("a removed column back in the file: valid, listed as added (not certified) and as removed",
   bk$valid && identical(bk$added_columns, "ip_address") && identical(bk$removed_columns, "ip_address"))
# Printed, it is named once: in the file, not certified - not also as missing (the case of 20.10.2026).
bk_shown <- capture.output(print(bk))
ok("printed once: in the file but not certified, nothing missing",
   sum(grepl("ip_address", bk_shown, fixed = TRUE)) == 1L &&
     any(bk_shown == paste0(strrep(" ", 17), "left out when the certificate was issued, so it cannot check them: ip_address")) &&
     any(bk_shown == "Missing:         none"))

# Cells are listed by row, then by column name in code point order - never by the locale's
# collation, which would put "comma,name" before "Spalte ...".
three <- d$cells
three[2, c(5, 7, 8)] <- c("3.1", "na", "y")
three_cells <- tmp(".csv")
write_csv_table(three_cells, d$header, three)
tc <- verify(three_cells)
ok("modified cells are sorted by row, then by code point of the column name",
   identical(tc$modified_cells$column, c("Spalte mit \u00dcmlaut", "comma,name", "q_num")) && all(tc$modified_cells$row == 2))

no_bom <- tmp(".csv")
writeBin(bytes_of(fx("dataset.csv"))[-(1:3)], no_bom)
ok("without the byte order mark: still valid", verify(no_bom)$valid)
windows <- tmp(".csv")
# Every line end between records (each is followed by the next row's number) becomes CRLF; the
# line breaks inside quoted cells stay as they are.
write_text(windows, gsub("\n(?=[0-9])", "\r\n", dataset_text, perl = TRUE))
ok("with CRLF between the rows (as Windows tools write it): still valid", verify(windows)$valid)

# What a spreadsheet program (or a data frame) does on "save": what looks like a number
# becomes one and is written back the way numbers are written.
as_saved <- function(x) {
  v <- as.character(utils::type.convert(x, as.is = TRUE, na.strings = character(0)))
  v[is.na(v)] <- ""
  v
}
roundtrip <- tmp(".csv")
saved <- d$cells
for (j in seq_len(ncol(saved))) saved[, j] <- as_saved(saved[, j])
write_csv_table(roundtrip, d$header, saved)
rt <- verify(roundtrip)
ok("numbers rewritten as a spreadsheet saves them: modified", rt$status == "modified")
ok("q_num is among the columns it names (007 became 7, 3.10 became 3.1)", "q_num" %in% rt$modified_cells$column)

fails_with("a data frame instead of the file is refused, with the reason", "A data frame will not do",
           sondavi_verify(data.frame(q_num = 7), fx("dataset.certificate.json"), trust = TRUST, keys = KEYS))
fails_with("a missing file says which", "No such file", verify(tmp(".csv")))

cat("\nA record that does not fit is never valid\n")
# The same six cases as the platform's and the browser's verifier: each must end in exactly this
# status, with a first problem that says this - in the words all four verifiers use.
never_valid <- function(label, data, certificate, status, text) {
  v <- verify(data, certificate)
  ok(sprintf("%s: %s, \"%s\"", label, status, text),
     identical(v$status, status) && length(v$problems) >= 1 && grepl(text, v$problems[1], fixed = TRUE))
}
never_valid("1. a manifest that is no JSON", fx("dataset.csv"),
            edited_certificate(function(cert) { cert$manifest <- "not json"; cert }), "malformed", "readable manifest")
never_valid("2. a cell certificate without its record", fx("dataset.csv"),
            edited_certificate(function(cert) { cert$proof <- NULL; cert }), "malformed", "supported version")
never_valid("3. a column of the record renamed", fx("dataset.csv"),
            edited_certificate(function(cert) {
              stopifnot(identical(cert$proof$columns[[4]]$name, "q_text"))
              cert$proof$columns[[4]]$name <- "q_other"
              cert
            }), "unverifiable", "does not match its manifest")
never_valid("4. a leaf of the removed column missing", fx("dataset.csv"),
            edited_certificate(function(cert) {
              at <- which(vapply(cert$proof$columns, function(col) identical(col$name, "ip_address"), NA))
              cert$proof$columns[[at]]$leaves <- utils::head(cert$proof$columns[[at]]$leaves, -1)
              cert
            }), "unverifiable", "removed column \"ip_address\" is incomplete")
never_valid("5. a salt of a column missing", fx("redacted.csv"),
            edited_redacted(function(cert) {
              stopifnot(identical(cert$proof$columns[[1]]$name, "response_id"))
              cert$proof$columns[[1]]$salts <- utils::head(cert$proof$columns[[1]]$salts, -1)
              cert
            }), "unverifiable", "column \"response_id\" is incomplete")
twice_named <- tmp(".csv")
write_csv_table(twice_named, c(d$header, "q_num"), cbind(d$cells, c("1", "2", "3", "4")))
never_valid("6. a column name used twice", twice_named, fx("dataset.certificate.json"), "unverifiable", "duplicate column names")

cat("\nA file you had certified (the certificate of a whole file)\n")
FILE_CERT <- fx("file.certificate.json")
wf <- verify(fx("dataset.csv"), FILE_CERT)
ok("the certified file: valid, a certificate of the whole file",
   wf$status == expected$file$status && wf$valid && wf$kind == "file")
ok("what the platform's comparison found: ip_address not in the file, 2 responses left out, no own columns",
   identical(wf$removed_columns, unlist(expected$file$removed_columns)) &&
     wf$removed_rows == expected$file$removed_rows && length(wf$added_columns) == 0)
ok("no cells, no problems", nrow(wf$modified_cells) == 0 && length(wf$problems) == 0)
ok("a cell certificate says it is one", verify(fx("dataset.csv"))$kind == "cells")
shown <- capture.output(print(wf))
ok("printed: it is the certified file", grepl("^Valid: this is the file that was certified", shown[1]))
ok("printed: 4 of 6 responses, 2 incomplete, group B",
   any(grepl("4 of 6 responses of this survey", shown, fixed = TRUE)) &&
     any(grepl("2 responses: 2 incomplete", shown, fixed = TRUE)) && any(grepl("by group: +B: 2", shown)))
ok("printed: the column not in the file, the notation, the row order and the format",
   any(shown == paste0(strrep(" ", 17), "in the survey's data, not in this file: ip_address")) && any(grepl("q_num (1 cell)", shown, fixed = TRUE)) &&
     any(grepl("Row order: +as in the survey's data$", shown)) &&
     any(grepl("File: +dataset.csv \\(365 bytes\\)$", shown)) &&
     any(grepl("Read as: +CSV, values separated by commas, UTF-8$", shown)) &&
     any(grepl("Rows matched: +by response ID$", shown)))

fingerprint <- "This is not the file that was certified: its fingerprint differs. Any change counts, even saving it again."
wt <- verify(fx("tampered.csv"), FILE_CERT)
ok("one cell changed: modified, with the one sentence and no cells",
   wt$status == expected$file_tampered$status && identical(wt$problems, fingerprint) && nrow(wt$modified_cells) == 0)
ok("printed: the one sentence", grepl("^Modified: this is not the file that was certified", capture.output(print(wt))[1]))
ok("printed: compared with the stored responses, no earlier certificate named",
   any(grepl("How: +Certified from the researcher's own CSV file, after every value was compared with the stored responses", shown)) &&
     !any(grepl("Derived from", shown, fixed = TRUE)))

# A file certified against an earlier certificate (the platform's "compared with the certified
# file"): fewer columns and rows than the earlier one, which may itself be gone from the platform.
dv <- verify(fx("derived.csv"), fx("derived.certificate.json"))
ok("a certificate derived from an earlier one: valid, a certificate of the whole file",
   dv$status == expected$derived$status && dv$kind == "file")
ok("it lists what is not in the file, counted against the survey",
   identical(dv$removed_columns, unlist(expected$derived$removed_columns)) &&
     dv$removed_rows == expected$derived$removed_rows && identical(dv$added_columns, character()))
from <- dv$manifest$basis$derived_from
ok("and names the certificate it was derived from",
   identical(from[names(expected$derived_from)], expected$derived_from))
ok("whose file is the platform's dataset.csv, by its fingerprint",
   identical(from$sha256, paste(as.character(unclass(openssl::sha256(bytes_of(fx("dataset.csv"))))), collapse = "")))
derived_shown <- capture.output(print(dv))
ok("printed: compared with a file certified earlier, in the verify page's words",
   any(grepl("How: +Certified from the researcher's own CSV file, after every value was compared with a file certified earlier", derived_shown)))
ok("printed: Derived from: certificate 00000000 of 2026-10-20 (dataset.csv)",
   any(grepl("^Derived from: +certificate 00000000 of 2026-10-20 \\(dataset.csv\\)$", derived_shown)))
# What a certificate says about where values came from, in the verify page's words. The fixture
# certificates name nothing of the kind, so these are re-signed with the throwaway key.
computed_note <- "worked out by the platform when the file was made, never certified"
browser_note <- "certified as stored, but sent by the participant's browser rather than measured by the platform"
with_extra <- tmp(".csv")
write_csv_table(with_extra, c(d$header, "my_score", "attention_check_passed"),
                cbind(d$cells, c("1", "2", "3", "4"), c("true", "true", "false", "true")))
export_cert <- resigned(function(m) {
  m$basis <- list(kind = "export", columns_derived = list("attention_check_passed"),
                  browser_reported = list("response_status", "q_num"), offline_synced = 2L)
  m
})
ex <- verify(with_extra, export_cert, keys = own_keys)
ex_shown <- capture.output(print(ex))
ok("a download with a computed column: valid, the column is not certified",
   ex$valid && identical(ex$added_columns, c("my_score", "attention_check_passed")))
ok("printed: the computed column as not certified, with the reason",
   any(ex_shown == "Not certified:   2 columns") &&
     any(ex_shown == paste0(strrep(" ", 17), computed_note, ": attention_check_passed")))
ok("and not among the columns added afterwards",
   any(ex_shown == paste0(strrep(" ", 17), "added after the certificate was issued: my_score")))
ok("printed: the columns the browser reported, with the note",
   any(grepl("^Sent by browser: response_status, q_num$", ex_shown)) &&
     any(ex_shown == paste0(strrep(" ", 17), browser_note)))
ok("printed: the responses from offline devices",
   any(grepl("^From offline devices: 2 responses collected offline; the time they were completed is the device's$", ex_shown)))

file_cert2 <- resigned(function(m) {
  m$basis$columns_own <- list("q_score", "my_own")
  m$basis$columns_derived <- list("q_score")
  m$basis$browser_reported <- list("response_status")
  m$basis$offline_synced <- 1L
  m$basis$matched_by <- "content"
  m$basis$file$format <- list(type = "csv", delimiter = ";", encoding = "Windows-1252")
  m
}, source = FILE_CERT)
fc_shown <- capture.output(print(verify(fx("dataset.csv"), file_cert2, keys = own_keys)))
ok("a certified file: read as CSV with semicolons, in its encoding",
   any(grepl("^Read as: +CSV, values separated by semicolons, Windows-1252$", fc_shown)))
ok("rows matched by their content",
   any(grepl("^Rows matched: +by their content \\(the file has no response ID\\)$", fc_shown)))
ok("a computed column is not one of the own columns",
   any(fc_shown == paste0(strrep(" ", 17), "not part of the survey's data, added by the researcher: my_own")) &&
     any(fc_shown == paste0(strrep(" ", 17), computed_note, ": q_score")))
ok("browser-reported and offline lines for a certified file too",
   any(grepl("^Sent by browser: response_status$", fc_shown)) &&
     any(grepl("^From offline devices: 1 response collected offline; the time they were completed is the device's$", fc_shown)))
read_as_fn <- if (INSTALLED) getFromNamespace("read_as", "sondavi") else read_as
ok("read as: commas, tabs, Excel with its sheet, JSON",
   read_as_fn(list(type = "csv", delimiter = ",", encoding = "UTF-8")) == "CSV, values separated by commas, UTF-8" &&
     read_as_fn(list(type = "csv", delimiter = "tab", encoding = "UTF-8")) == "CSV, values separated by tabs, UTF-8" &&
     read_as_fn(list(type = "xlsx", sheet = "Responses")) == "Excel, sheet \"Responses\"" &&
     read_as_fn(list(type = "json")) == "JSON, one object per response")
ok("a derived certificate does not fit another file", verify(fx("dataset.csv"), fx("derived.certificate.json"))$status == "modified")
# What a chain of certificates may not know: the status or group of a response no file named.
unknown_basis <- list(basis = list(kind = "file", file = list(name = "x.csv", format = list(type = "csv")),
  responses_total = 6L, responses_in_file = 3L,
  missing = list(count = 3L, by_status = list(partial = 1L, unknown = 2L), by_group = list(A = 1L), group_unknown = 2L)))
fake <- structure(list(status = "valid", kind = "file", removed_rows = 3L, removed_columns = character(),
                       added_columns = character(), manifest = unknown_basis), class = "sondavi_verification")
fl <- if (INSTALLED) getFromNamespace("file_lines", "sondavi")(fake) else file_lines(fake)
ok("a status no file names reads 'of unknown status'", any(grepl("3 responses: 1 incomplete, 2 of unknown status", fl, fixed = TRUE)))
ok("and a group no file names is counted after the named ones",
   any(grepl("by group: +A: 1, and 2 of a group no file names", fl)))
ok("a file with other line ends is another file (a cell certificate would still check)",
   verify(windows, FILE_CERT)$status == "modified" && verify(windows)$valid)
as_excel <- tmp(".xlsx")
writeBin(bytes_of(fx("dataset.csv")), as_excel)
ok("the format is never read: the same bytes under another extension are the certified file", verify(as_excel, FILE_CERT)$valid)
binary <- tmp(".xlsx")
writeBin(as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x00, 0x00, 0xff, 0x22)), binary)
bn <- verify(binary, FILE_CERT)
ok("a binary file is hashed, not parsed: modified, never unreadable", bn$status == "modified" && identical(bn$problems, fingerprint))
packed <- tmp(".csv.gz")
con <- gzfile(packed, "wb"); writeBin(bytes_of(fx("dataset.csv")), con); close(con)
ok("a compressed copy is not unpacked before hashing: another file", verify(packed, FILE_CERT)$status == "modified")
ok("from an untrusted issuer: untrusted_issuer, nothing hashed or fetched",
   sondavi_verify(fx("dataset.csv"), FILE_CERT)$status == "untrusted_issuer")
file_edited <- tmp(".certificate.json")
fc <- jsonlite::fromJSON(FILE_CERT, simplifyVector = FALSE)
fc$manifest <- sub("\"count\":2", "\"count\":0", fc$manifest, fixed = TRUE)
write_text(file_edited, as.character(jsonlite::toJSON(fc, auto_unbox = TRUE, null = "null", digits = NA)))
ok("its record edited (2 responses left out made 0): invalid_signature", verify(fx("dataset.csv"), file_edited)$status == "invalid_signature")
no_proof <- edited_certificate(function(cert) { cert["proof"] <- list(NULL); cert })
ok("a cell certificate without its record stays malformed - only a file certificate has none",
   identical(verify(fx("dataset.csv"), no_proof)$problems, "This is not a certificate of a supported version."))
fails_with("redacting a certified file is refused: certify the changed file again", "certify the changed file again on the platform",
           sondavi_redact(fx("dataset.csv"), FILE_CERT, drop_rows = 1, out_data = tmp(".csv"), trust = TRUST, keys = KEYS))

cat("\nKeys built into the package\n")
# The package carries the keys of https://sondavi.io (inst/certificate-keys.json). Here the
# built-in document and the fetch from the issuer are replaced, so nothing goes over the network;
# the fixture issuer stands in for sondavi.io.
reads <- 0L
fetches <- 0L
builtin_for <- function(url, with_key) function() {
  reads <<- reads + 1L
  list(issuer = list(name = "Sondavi", url = url), keys = if (with_key) fixture_keys$keys else list(),
       saved_from = NULL, saved_at = NULL)
}
fetch_fails <- function(url, what, hint = "") {
  fetches <<- fetches + 1L
  stop("Could not fetch ", what, " from ", url, ": offline.", hint, call. = FALSE)
}
fetch_fixture <- function(url, what, hint = "") {
  fetches <<- fetches + 1L
  jsonlite::fromJSON(KEYS, simplifyVector = FALSE)
}
# An error comes back as the condition, so a broken lookup fails its own check below instead
# of ending the script.
offline <- function(builtin, fetch, code) {
  reads <<- 0L
  fetches <<- 0L
  tryCatch(with_replaced(list(builtin_key_document = builtin, fetch_json = fetch), code), error = identity)
}
is_valid <- function(x) inherits(x, "sondavi_verification") && isTRUE(x$valid)
status_of <- function(x) if (inherits(x, "sondavi_verification")) x$status else conditionMessage(x)

b_valid <- offline(builtin_for(TRUST, TRUE), fetch_fails,
                   sondavi_verify(fx("dataset.csv"), fx("dataset.certificate.json"), trust = TRUST))
ok("a) the key built in for the issuer: valid without any connection",
   is_valid(b_valid) && fetches == 0L)
b_fetched <- offline(builtin_for(TRUST, FALSE), fetch_fixture,
                     sondavi_verify(fx("dataset.csv"), fx("dataset.certificate.json"), trust = TRUST))
ok("b) a key that is not built in (a newer one): fetched from the issuer, once - valid",
   is_valid(b_fetched) && fetches == 1L)
b_other_msg <- status_of(offline(builtin_for("https://another.example", TRUE), fetch_fails,
                                 sondavi_verify(fx("dataset.csv"), fx("dataset.certificate.json"), trust = TRUST)))
ok("c) keys built in for another issuer are not used for this one: its keys are fetched",
   is.character(b_other_msg) && fetches == 1L && grepl("Could not fetch", b_other_msg, fixed = TRUE))
ok("   and when that fails, the message says what a newer key needs",
   grepl("a newer key", b_other_msg, fixed = TRUE) && grepl("keys =", b_other_msg, fixed = TRUE))
b_given <- offline(builtin_for(TRUST, TRUE), fetch_fails,
                   sondavi_verify(fx("dataset.csv"), fx("dataset.certificate.json"), trust = TRUST, keys = list(keys = list())))
ok("d) keys given: only those count, the built-in ones not at all - unknown_key",
   status_of(b_given) == "unknown_key" && fetches == 0L && reads == 0L)
b_untrusted <- offline(builtin_for(TRUST, TRUE), fetch_fixture,
                       sondavi_verify(fx("dataset.csv"), fx("dataset.certificate.json")))
ok("e) an untrusted issuer: nothing fetched, the built-in keys not even read - untrusted_issuer",
   status_of(b_untrusted) == "untrusted_issuer" && fetches == 0L && reads == 0L)
# A built-in entry passes the same test as a fetched one: the fixture's id with another key is
# no key, so the real one is fetched - taken as it is, it would make the signature "invalid".
b_impostor <- offline(function() {
  reads <<- reads + 1L
  list(issuer = list(name = "Sondavi", url = TRUST), keys = list(key_entry(fixture_keys$keys[[1]]$id, other_pub)))
}, fetch_fixture, sondavi_verify(fx("dataset.csv"), fx("dataset.certificate.json"), trust = TRUST))
ok("a built-in key under an id that is not its own is no key: the real one is fetched - valid",
   is_valid(b_impostor) && fetches == 1L)
b_missing <- offline(function() stop("cannot read the file"), fetch_fixture,
                     sondavi_verify(fx("dataset.csv"), fx("dataset.certificate.json"), trust = TRUST))
ok("an unreadable built-in file counts as no keys: fetched instead, valid", is_valid(b_missing) && fetches == 1L)
b_redact <- offline(builtin_for(TRUST, TRUE), fetch_fails,
                    sondavi_redact(fx("dataset.csv"), fx("dataset.certificate.json"), drop_rows = 1,
                                   out_data = tmp(".csv"), trust = TRUST))
ok("redacting with the built-in key: valid, without any connection", is_valid(b_redact) && fetches == 0L)
b_redact2 <- offline(builtin_for(TRUST, FALSE), fetch_fixture,
                     sondavi_redact(fx("dataset.csv"), fx("dataset.certificate.json"), drop_rows = 1,
                                    out_data = tmp(".csv"), trust = TRUST))
ok("redacting checks twice, but fetches the issuer's keys once", is_valid(b_redact2) && fetches == 1L)
# The lookup itself: a key found nowhere does not send it to the issuer a second time.
key_ring_fn <- if (INSTALLED) getFromNamespace("key_ring", "sondavi") else key_ring
twice <- offline(builtin_for(TRUST, FALSE), fetch_fixture, {
  ring <- key_ring_fn(NULL)
  list(ring(TRUST, "ed25519:0000000000000000"), ring(TRUST, "ed25519:0000000000000000"),
       ring(TRUST, fixture_keys$keys[[1]]$id))
})
ok("the issuer's keys are fetched once per check, also for a key that is not among them",
   is.list(twice) && is.null(twice[[1]]) && is.null(twice[[2]]) && length(twice[[3]]) == 32L && fetches == 1L)

# f) The file that ships with the package, as it is.
shipped <- if (INSTALLED) system.file("certificate-keys.json", package = "sondavi") else file.path(pkg_root, "inst", "certificate-keys.json")
ships <- jsonlite::fromJSON(shipped, simplifyVector = FALSE)
ok("f) the shipped keys document is read as the issuer https://sondavi.io, exactly",
   identical(ships$issuer$url, "https://sondavi.io"))
ok("   every key in it is a key under its own id (none is dropped)",
   is.list(ships$keys) && length(keys_by_id(ships)) == length(ships$keys))
if (INSTALLED) ok("   and it is what the package reads", identical(builtin_key_document(), ships))

cat("\nLeaving columns and rows out\n")
out <- tmp(".csv")
res <- sondavi_redact(fx("dataset.csv"), fx("dataset.certificate.json"),
                      drop_columns = unlist(expected$redaction$drop_columns),
                      drop_rows = unlist(expected$redaction$drop_rows),
                      out_data = out, trust = TRUST, keys = KEYS)
mine <- jsonlite::fromJSON(res$files[["certificate"]], simplifyVector = FALSE)
php <- jsonlite::fromJSON(fx("redacted.certificate.json"), simplifyVector = FALSE)
ok("the certificate goes next to the file, named after it",
   res$files[["certificate"]] == sub("[.]csv$", ".certificate.json", out))
ok("the record is the platform's, entry for entry", identical(mine$proof, php$proof))
ok("the manifest is passed on as the identical text, and the signature unchanged",
   identical(mine$manifest, php$manifest) && identical(mine$signature, php$signature))
ok("the file is byte for byte the platform's", identical(bytes_of(out), bytes_of(fx("redacted.csv"))))
ok("the result checks as valid", res$valid && identical(res$removed_columns, c("respondent_id", "ip_address")) && res$removed_rows == 1)
ok("and is checked again from the files", verify(out, res$files[["certificate"]])$valid)
ok("the certificate is written compactly", !grepl("\n ", text_of(res$files[["certificate"]])))

# Again, on the redacted pair: a column whose rows are not all there any more.
again <- sondavi_redact(out, res$files[["certificate"]], drop_columns = "q_multi", drop_rows = 1,
                        out_data = tmp(".csv"), trust = TRUST, keys = KEYS)
ok("redacting a redacted file again: valid, 2 rows and 3 columns left out",
   again$valid && again$removed_rows == 2 && identical(again$removed_columns, c("respondent_id", "q_multi", "ip_address")))
proof <- jsonlite::fromJSON(again$files[["certificate"]], simplifyVector = FALSE)$proof
entries <- unlist(lapply(proof$columns, function(col) col$salts))
ok("no seed is left once rows are missing", !any(vapply(proof$columns, function(col) "seed" %in% names(col), NA)))
salted <- Filter(function(col) !is.null(col$salts), proof$columns)
ok("a removed row keeps only its leaf (\"-\" + 64 hex), a present one its salt (32 hex)",
   all(grepl("^(-[0-9a-f]{64}|[0-9a-f]{32})$", entries)) && length(salted) == 6 &&
     all(vapply(salted, function(col) sum(startsWith(unlist(col$salts), "-")) == 2, NA)))
ok("the rows of the file point at their original rows", identical(unlist(proof$rows), c(2L, 3L)))

none <- sondavi_redact(fx("dataset.csv"), fx("dataset.certificate.json"), drop_rows = 1:4,
                       out_data = tmp(".csv"), trust = TRUST, keys = KEYS)
ok("every row left out: a header that still checks as valid", none$valid && none$removed_rows == 4)
own <- sondavi_redact(added, fx("dataset.certificate.json"), drop_rows = 3, out_data = tmp(".csv"), trust = TRUST, keys = KEYS)
ok("a column of one's own is carried along", own$valid && identical(own$added_columns, "my_score"))

fails_with("a modified file is not redacted", "this one is \"modified\"",
           sondavi_redact(fx("tampered.csv"), fx("dataset.certificate.json"), drop_rows = 1,
                          out_data = tmp(".csv"), trust = TRUST, keys = KEYS))
fails_with("nor one from an untrusted issuer", "this one is \"untrusted_issuer\"",
           sondavi_redact(fx("dataset.csv"), fx("dataset.certificate.json"), drop_rows = 1,
                          out_data = tmp(".csv"), keys = KEYS))
fails_with("a column that is not there is named", "No column \"nope\" in the file.",
           sondavi_redact(fx("dataset.csv"), fx("dataset.certificate.json"), drop_columns = "nope",
                          out_data = tmp(".csv"), trust = TRUST, keys = KEYS))
fails_with("so is a row that is not there", "No data row 5 in the file.",
           sondavi_redact(fx("dataset.csv"), fx("dataset.certificate.json"), drop_rows = 5,
                          out_data = tmp(".csv"), trust = TRUST, keys = KEYS))
fails_with("the originals are never overwritten", "the originals stay as they are",
           sondavi_redact(fx("dataset.csv"), fx("dataset.certificate.json"), drop_rows = 1,
                          out_data = fx("dataset.csv"), trust = TRUST, keys = KEYS))
fails_with("leaving out every column is refused", "nothing to check",
           sondavi_redact(fx("dataset.csv"), fx("dataset.certificate.json"), drop_columns = HEADER,
                          out_data = tmp(".csv"), trust = TRUST, keys = KEYS))

cat("\n", checks - failures, "/", checks, " checks passed\n", sep = "")
if (failures) quit(status = 1)
