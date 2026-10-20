# Certified datasets, format version 1: checking a data file against the certificate the
# platform issued for it. Leaving columns or rows out is the platform's job (it certifies the
# changed file again); the offline way the format allows stays here, internal, as its reference.
#
# The platform's own verifier is the reference. This file follows it step by step - the same
# statuses, the same order of checks, the same messages - so the browser, R and Python give
# the same answer for the same pair of files. A change to the hashing below is a new format
# version, never an edit.

CERTIFICATE_TYPE <- "sondavi-certificate"
MANIFEST_TYPE <- "sondavi-certificate-manifest"
KEYS_PATH <- "/.well-known/sondavi-certificate-keys.json"

#' Check a certified dataset against its certificate
#'
#' The platform can issue a dataset with a certificate: a CSV file plus a
#' `<name>.certificate.json`, signed by the platform. The certificate vouches
#' that every cell it covers holds exactly what the participants entered. This
#' function checks that on your own computer - no account needed, and no need
#' to trust whoever passed the files on.
#'
#' Both arguments are **file paths, not a data frame**. Reading a CSV into a data
#' frame changes the text: `"007"` becomes `7`, `"NA"` becomes a missing value,
#' `"3.10"` becomes `3.1`. The certificate is about the text as it was
#' delivered, so the file is read here byte for byte, every cell as the exact
#' text between its commas - and strictly, as every verifier of the platform
#' reads it: valid UTF-8, quotes only around a whole field, line ends LF or
#' CRLF, every row as wide as the header. A file another program could read
#' differently is `"unverifiable"`.
#'
#' Adding columns of your own keeps the certificate valid; the result lists
#' them, and any certified columns or rows that were left out. Changing a value,
#' inventing, deleting or reordering rows, or deleting a column by hand is
#' found. So is **opening the file in a spreadsheet program and saving it
#' again**: that rewrites numbers and dates (`007` becomes `7`, `-0` becomes
#' `0`), and the cells concerned are reported as modified. Pass the file on as
#' it was downloaded.
#'
#' To share a version with fewer columns or rows, or in another format, certify
#' the changed file on the platform that issued the certificate ("Certify a file
#' you have", compared with the certified file). That works even after the
#' responses were deleted there.
#'
#' Trust is explicit. Anyone can run an installation of the platform with a key
#' of their own, so a certificate is only accepted from the issuers listed in
#' `trust`; any other issuer gives `"untrusted_issuer"`, and nothing is checked.
#'
#' There are two kinds of certificate. One is issued with a download from the
#' platform and covers each cell (`kind` `"cells"`). The other certifies a file
#' you already had - CSV, Excel or JSON - after the platform compared every
#' value with the stored responses, or with a file certified earlier (`kind`
#' `"file"`): it vouches for the file as a whole, through the SHA-256 of its
#' bytes, and records what the comparison found. Such a file is not read here,
#' only hashed, so any change to it - even opening and saving it again - makes
#' it `"modified"`.
#'
#' The checks run in this order, and the first that fails decides the status:
#' \describe{
#'   \item{`"malformed"`}{The certificate file is not a certificate of a supported version.}
#'   \item{`"untrusted_issuer"`}{It was issued by an installation not in `trust`.}
#'   \item{`"unknown_key"`}{The issuer's published keys do not include the one it names.}
#'   \item{`"invalid_signature"`}{The certificate itself was altered after it was issued.}
#'   \item{`"unverifiable"`}{The file does not have the structure the certificate
#'     describes: a certified column is missing without being listed as removed,
#'     the number of rows does not match, a row has more or fewer fields than
#'     the header, or it is not a well-formed CSV file.}
#'   \item{`"modified"`}{The certificate is genuine, but cells differ from what was certified.}
#'   \item{`"valid"`}{Every certified cell matches.}
#' }
#'
#' @param data Path of the data file: the CSV file, or for a certified file of
#'   your own the file itself (CSV, Excel or JSON).
#' @param certificate Path of its certificate, usually `<name>.certificate.json`.
#' @param trust The addresses of the issuers whose certificates you accept.
#'   Only `"https://sondavi.io"` by default. For a certificate from another
#'   installation, pass its address - the `README.txt` of a certified download
#'   names it - and only if you trust that installation.
#' @param keys The issuer's public keys. `NULL` (the default) first looks among
#'   the keys of `https://sondavi.io` built into the package - those known when
#'   this version was released - and fetches the issuer's published keys, from
#'   `<issuer>/.well-known/sondavi-certificate-keys.json`, only for a key that is
#'   not built in: a newer one, or one of another installation. Either happens
#'   only once the issuer has been found in `trust`. Otherwise the path or URL of
#'   a keys document, or the document already read with [jsonlite::fromJSON()];
#'   then only those keys are used, the built-in ones not at all.
#'   A key is used only under its own id (`ed25519:` + the first 16 hex digits
#'   of SHA-256 over the key); any other entry is ignored.
#' @return An object of class `sondavi_verification`, printed as a short
#'   summary. It is a list with
#'   \describe{
#'     \item{`status`}{One of the statuses above.}
#'     \item{`valid`}{`TRUE` when the status is `"valid"`.}
#'     \item{`kind`}{`"cells"` for a certificate of each cell, `"file"` for a
#'       certified file as a whole.}
#'     \item{`removed_columns`}{Certified columns left out of the file. Their
#'       names are in the certificate, their content is not.}
#'     \item{`removed_rows`}{How many certified rows were left out.}
#'     \item{`added_columns`}{Columns of the file the certificate does not cover.}
#'     \item{`modified_cells`}{A data frame with `row` (the data row of the file,
#'       counted from 1 after the header) and `column` for every cell that differs.}
#'     \item{`suspect`}{`TRUE` when cells differ in several rows and several
#'       columns. The certificate holds one sum per row and one per column, not
#'       each cell, so `modified_cells` are then the cells where a differing row
#'       meets a differing column: suspects, not all of them changed.}
#'     \item{`problems`}{What else went wrong, in words.}
#'     \item{`certificate`}{The certificate's id.}
#'     \item{`issuer`}{`name` and `url` of the installation that issued it.}
#'     \item{`organization`}{The organisation whose study it is.}
#'     \item{`survey`}{`id`, `title`, `version` (of the questionnaire),
#'       `definition_sha256` and `privacy` of the study.}
#'     \item{`issued_at`}{When the certificate was issued, as `POSIXct` in UTC.}
#'     \item{`manifest`}{Everything the certificate states, as signed. For a
#'       certified file, `manifest$basis` holds what the platform's comparison
#'       found: responses in the file and left out (by status and group),
#'       columns checked, not in the file and of your own, values written in
#'       another notation, the row order and the file's format - and, when it
#'       was compared with a file certified earlier, which one (`derived_from`).
#'       For both kinds, `manifest$basis` may name columns computed rather than
#'       certified (`columns_derived`), values the participant's browser reported
#'       (`browser_reported`) and responses collected offline (`offline_synced`);
#'       the printed summary shows them.}
#'     \item{`files`}{The two paths that were checked.}
#'   }
#'   Who issued it, for which study and when is only established when the
#'   status is `"valid"`, `"modified"` or `"unverifiable"`; for the other
#'   statuses these fields repeat what the certificate claims, unchecked.
#' @examples
#' \dontrun{
#' v <- sondavi_verify("wave2.csv", "wave2.certificate.json")
#' v
#' #> Valid: every certified cell matches.
#' #> Survey:          Wave 2 follow-up (id 42, questionnaire version 3, pseudonymized)
#' #> ...
#' stopifnot(v$valid)
#'
#' # A certificate from another installation, with its keys at hand
#' sondavi_verify("data.csv", "data.certificate.json",
#'                trust = "https://survey.example.org", keys = "keys.json")
#' }
#' @export
sondavi_verify <- function(data, certificate, trust = "https://sondavi.io", keys = NULL) {
  check_input_file(data, "data")
  check_input_file(certificate, "certificate")
  trust <- check_trust(trust)

  checked <- verify_pair(data, read_certificate(certificate), trust, key_ring(keys))
  as_verification(checked$result, data, certificate)
}

#' Leave columns or rows out of a certified dataset, offline
#'
#' Not offered: the packages only verify. To share fewer columns or rows, the
#' platform certifies the changed file again (SPECS 86.5 and 86.9). This is the
#' offline way the published format allows (docs/CERTIFICATE-FORMAT.md, section 7), kept
#' as the reference for that description and for the tests: it writes the file
#' without the given columns and rows and a certificate that fits it, with the
#' platform's signature unchanged. Removed cells keep only a hash with a random
#' salt, never the salt. The input has to check as valid first; a certificate of
#' a whole file has no record of single cells and is refused.
#'
#' @param data,certificate Paths of the CSV file and its certificate.
#' @param drop_columns Names of the columns to leave out.
#' @param drop_rows Data rows to leave out, numbered as in the file (from 1).
#' @param out_data,out_certificate Where the new pair goes; the certificate by
#'   default next to `out_data`, named `<name>.certificate.json`.
#' @param trust,keys As in [sondavi_verify()].
#' @return Invisibly, the check of the new pair, with the two paths in `files`.
#' @noRd
sondavi_redact <- function(data, certificate, drop_columns = character(), drop_rows = integer(),
                           out_data, out_certificate = sub("([.]csv)?$", ".certificate.json", out_data, ignore.case = TRUE),
                           trust = "https://sondavi.io", keys = NULL) {
  check_input_file(data, "data")
  check_input_file(certificate, "certificate")
  if (missing(out_data) || !is_string(out_data) || !nzchar(out_data)) {
    stop("Where should the new file go? Pass `out_data`.", call. = FALSE)
  }
  if (!is_string(out_certificate) || !nzchar(out_certificate)) {
    stop("`out_certificate` must be a file path.", call. = FALSE)
  }
  sources <- normalizePath(c(data, certificate), mustWork = TRUE)
  targets <- normalizePath(c(out_data, out_certificate), mustWork = FALSE)
  if (any(targets %in% sources) || targets[1] == targets[2]) {
    stop("Write the redacted copy to new files; the originals stay as they are.", call. = FALSE)
  }
  trust <- check_trust(trust)
  if (!is.character(drop_columns) || anyNA(drop_columns)) {
    stop("`drop_columns` must be column names.", call. = FALSE)
  }
  if (!is.numeric(drop_rows) || anyNA(drop_rows) || any(drop_rows != round(drop_rows))) {
    stop("`drop_rows` must be row numbers.", call. = FALSE)
  }

  ring <- key_ring(keys)
  cert <- read_certificate(certificate)
  manifest <- tryCatch(jsonlite::parse_json(field(cert, "manifest"), simplifyVector = FALSE), error = function(e) NULL)
  if (is_whole_file(manifest)) {
    stop("This certificate vouches for the file as a whole and holds no record of single cells, ",
         "so nothing can be left out of it without losing the certificate. Leave out what you need, ",
         "then certify the changed file again on the platform.", call. = FALSE)
  }
  checked <- verify_pair(data, cert, trust, ring)
  if (!identical(checked$result$status, "valid")) {
    problems <- checked$result$problems
    stop(
      "Only a dataset that checks as valid can be redacted; this one is \"",
      checked$result$status, "\".",
      if (length(problems)) paste0(" ", paste(problems, collapse = " ")),
      " See sondavi_verify().",
      call. = FALSE
    )
  }

  table <- checked$table
  for (name in drop_columns) {
    if (!name %in% table$header) stop("No column \"", name, "\" in the file.", call. = FALSE)
  }
  for (r in drop_rows) {
    if (r < 1 || r > nrow(table$cells)) stop("No data row ", r, " in the file.", call. = FALSE)
  }
  if (length(table$header) && all(table$header %in% drop_columns)) {
    stop("Leaving out every column leaves nothing to check.", call. = FALSE)
  }

  manifest <- jsonlite::parse_json(cert$manifest, simplifyVector = FALSE)
  redacted <- redact_record(cert, manifest, table, unique(drop_columns), unique(as.integer(drop_rows)))

  write_csv_table(out_data, redacted$header, redacted$cells)
  write_certificate(out_certificate, redacted$certificate)

  # The new pair must check as valid before anyone passes it on.
  again <- verify_pair(out_data, read_certificate(out_certificate), trust, ring)
  if (!identical(again$result$status, "valid")) {
    unlink(c(out_data, out_certificate))
    stop("The redacted copy did not check as valid (\"", again$result$status,
         "\"), so it was not kept. Please report this.", call. = FALSE)
  }
  invisible(as_verification(again$result, out_data, out_certificate))
}

#' @export
print.sondavi_verification <- function(x, ...) {
  cat(verification_lines(x), sep = "\n")
  invisible(x)
}

# -- the check, step by step as the platform's verifier does it ------------------------------

# The result of a check, and the table read on the way (sondavi_redact() writes from it).
verify_pair <- function(data, cert, trust, keys_for) {
  done <- function(status, manifest = NULL, problems = character(), table = NULL) {
    list(result = verification_result(status, manifest, problems = problems), table = table)
  }

  if (!is.list(cert) || !identical(field(cert, "type"), CERTIFICATE_TYPE) || !identical(field(cert, "version"), 1L) ||
      !is_string(field(cert, "manifest")) || !is_string(field(cert, "signature"))) {
    return(done("malformed", problems = "This is not a certificate of a supported version."))
  }
  manifest <- tryCatch(jsonlite::parse_json(cert$manifest, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(manifest) || !identical(field(manifest, "type"), MANIFEST_TYPE)) {
    return(done("malformed", problems = "The certificate does not contain a readable manifest."))
  }
  # A certified file has no record of single cells: its bytes are what was signed.
  whole_file <- is_whole_file(manifest)
  if (!whole_file && !is.list(field(cert, "proof"))) {
    return(done("malformed", problems = "This is not a certificate of a supported version."))
  }

  issuer <- trim_slash(text_or_empty(field(field(manifest, "issuer"), "url")))
  if (!issuer %in% trim_slash(trust)) return(done("untrusted_issuer", manifest))

  key <- keys_for(issuer, text_or_empty(field(manifest, "key_id")))
  if (is.null(key)) return(done("unknown_key", manifest))

  # The manifest text itself is signed, byte for byte - never a re-serialised copy of it.
  signature <- strict_base64(cert$signature)
  if (is.null(signature) || length(signature) != 64L ||
      !signature_ok(charToRaw(enc2utf8(cert$manifest)), signature, key)) {
    return(done("invalid_signature", manifest))
  }

  if (whole_file) return(list(result = compare_whole_file(manifest, data), table = NULL))

  table <- tryCatch(read_csv_table(data), sondavi_csv_error = function(e) NULL)
  if (is.null(table)) return(done("unverifiable", manifest, NOT_WELL_FORMED))
  result <- tryCatch(
    compare_with_record(manifest, cert$proof, table),
    error = function(e) verification_result("unverifiable", manifest, problems = "The certificate's record could not be read.")
  )
  list(result = result, table = table)
}

# A certificate of a file the researcher had (CSV, Excel or JSON): the platform compared its
# content with the stored responses and signed the SHA-256 of the file's bytes together with what
# the comparison found. So the file is hashed here, never read - whatever its format - and what
# it holds is what the signed manifest says.
is_whole_file <- function(manifest) identical(field(field(manifest, "basis"), "kind"), "file")

compare_whole_file <- function(manifest, path) {
  basis <- field(manifest, "basis")
  if (!identical(file_sha256(path), field(field(basis, "file"), "sha256"))) {
    return(verification_result("modified", manifest, problems =
      "This is not the file that was certified: its fingerprint differs. Any change counts, even saving it again."))
  }
  count <- field(field(basis, "missing"), "count")
  verification_result("valid", manifest, list(
    removed_columns = names_in(field(basis, "columns_not_in_file")),
    removed_rows = if (is.numeric(count) && length(count) == 1L && !is.na(count)) as.integer(count) else 0L,
    added_columns = names_in(field(basis, "columns_own"))
  ))
}

# Streamed, so a large Excel file is not read into memory at once. `raw = TRUE`: otherwise R
# quietly unpacks a gzip-compressed file and hashes what is inside.
file_sha256 <- function(path) raw_hex(unclass(openssl::sha256(file(path, raw = TRUE))))

names_in <- function(x) {
  x <- unlist(x, use.names = FALSE)
  if (is.character(x)) x else character()
}

compare_with_record <- function(manifest, proof, table) {
  unverifiable <- function(problem) verification_result("unverifiable", manifest, problems = problem)

  n <- as_count(field(manifest, "rows"))
  columns <- vapply(as_array(field(manifest, "columns")), function(col) text_or_na(field(col, "name")), "")
  proof_columns <- as_array(field(proof, "columns") %||% list())
  proof_rows <- as_array(field(proof, "rows") %||% list())
  header <- table$header
  cells <- table$cells

  if (anyDuplicated(header)) return(unverifiable("The data file has duplicate column names."))
  # A row longer than the header has values no column names: R and pandas read such a file shifted.
  wrong <- which(table$widths != length(header))[1]
  if (!is.na(wrong)) {
    return(unverifiable(sprintf("Data row %d has %d fields, the header %d.", wrong, table$widths[wrong], length(header))))
  }
  names_in_record <- vapply(proof_columns, function(entry) text_or_na(field(entry, "name")), "")
  if (length(names_in_record) != length(columns) || anyNA(names_in_record) || anyNA(columns) ||
      any(names_in_record != columns)) {
    return(unverifiable("The certificate's record does not match its manifest."))
  }
  if (length(proof_rows) != nrow(cells)) {
    return(unverifiable(sprintf(
      "The file has %d data rows, the certificate accounts for %d. To share fewer rows, certify the changed file again where the certificate was issued.",
      nrow(cells), length(proof_rows)
    )))
  }
  # file_row[i + 1]: the data row of the file holding original row i, NA if it was removed.
  file_row <- rep(NA_integer_, n)
  if (length(proof_rows)) {
    if (!all(vapply(proof_rows, function(o) is.integer(o) && length(o) == 1L && !is.na(o), NA))) {
      return(unverifiable("The certificate's row list is invalid."))
    }
    original <- unlist(proof_rows)
    if (any(original < 0L) || any(original >= n) || anyDuplicated(original)) {
      return(unverifiable("The certificate's row list is invalid."))
    }
    file_row[original + 1L] <- seq_along(original)
  }
  present <- !is.na(file_row)
  all_present <- sum(present) == n
  position <- match(columns, header)

  removed <- logical(length(proof_columns))
  leaves <- vector("list", length(proof_columns))
  for (c in seq_along(proof_columns)) {
    entry <- proof_columns[[c]]
    name <- columns[c]
    if (php_truthy(field(entry, "removed"))) {
      removed[c] <- TRUE
      recorded <- field(entry, "leaves") %||% list()
      if (!is.list(recorded)) stop("not a list of leaves")
      if (length(recorded) != n) {
        return(unverifiable(sprintf("The record of the removed column \"%s\" is incomplete.", name)))
      }
      leaves[[c]] <- lapply(recorded, hex_raw, bytes = 32L)
      next
    }
    if (is.na(position[c])) {
      return(unverifiable(sprintf(
        "The certified column \"%s\" is missing from the file. To share fewer columns, certify the changed file again where the certificate was issued.", name
      )))
    }
    values <- character(n)
    values[present] <- cells[file_row[present], position[c]]
    seed <- field(entry, "seed")
    if (!is.null(seed)) {
      if (!all_present) return(unverifiable("The certificate's record is inconsistent with the removed rows."))
      seed <- hex_raw(seed, 32L)
      salts <- lapply(seq_len(n) - 1L, function(i) salt_from_seed(seed, i))
      leaves[[c]] <- column_leaves(name, salts, values)
      next
    }
    recorded <- field(entry, "salts")
    if (!is.list(recorded) || length(recorded) != n) {
      return(unverifiable(sprintf("The record of the column \"%s\" is incomplete.", name)))
    }
    # Entry by entry, as the platform reads them: an entry that is no scalar cannot be read;
    # then whether it fits the rows of the file, then whether it is a salt or leaf at all. A
    # present row with only a leaf would leave its value unchecked; a removed row with a salt
    # has no value.
    nested <- vapply(recorded, is.list, NA)
    is_leaf <- startsWith(vapply(recorded, function(x) if (is.list(x)) "" else php_string(x), ""), "-")
    fits <- present != is_leaf
    readable <- vapply(seq_len(n), function(i) {
      item <- recorded[[i]]
      is_string(item) && is_hex(if (is_leaf[i]) substring(item, 2) else item, if (is_leaf[i]) 32L else 16L)
    }, NA)
    first <- which(nested | !fits | !readable)[1]
    if (!is.na(first)) {
      if (!nested[first] && !fits[first]) {
        return(unverifiable(sprintf("The record of the column \"%s\" is inconsistent with the rows in the file.", name)))
      }
      stop("unreadable record entry")
    }
    recorded <- unlist(recorded)
    out <- vector("list", n)
    out[is_leaf] <- lapply(substring(recorded[is_leaf], 2), hex_raw, bytes = 32L)
    out[!is_leaf] <- column_leaves(name, lapply(recorded[!is_leaf], hex_raw, bytes = 16L), values[!is_leaf])
    leaves[[c]] <- out
  }

  roots <- as_array(field(manifest, "columns"))
  bad_columns <- which(vapply(seq_along(proof_columns), function(c) {
    root <- field(roots[[c]], "root")
    if (is.null(root)) stop("a column without its sum")
    !identical(raw_hex(column_root(columns[c], leaves[[c]])), root)
  }, NA))
  row_roots <- field(manifest, "row_roots")
  bad_rows <- which(vapply(seq_len(n), function(i) {
    expected <- if (is.list(row_roots) && i <= length(row_roots)) row_roots[[i]] else NULL
    !identical(raw_hex(row_root(i - 1L, lapply(leaves, `[[`, i))), expected)
  }, NA))

  details <- list(
    removed_columns = columns[removed],
    removed_rows = as.integer(n - sum(present)),
    # A column the certificate lists as left out, back in the file, is not certified: its
    # values are new.
    added_columns = header[!header %in% columns[!removed]]
  )
  if (!length(bad_columns) && !length(bad_rows)) {
    return(verification_result("valid", manifest, details))
  }

  # At most one message, as on the platform: an altered record of removed data, or - when only
  # column sums or only row sums disagree - an altered record as such.
  at_row <- integer()
  at_column <- character()
  problem <- character()
  for (i in bad_rows) {
    for (c in bad_columns) {
      if (present[i] && !removed[c]) {
        at_row <- c(at_row, file_row[i])
        at_column <- c(at_column, columns[c])
      } else {
        problem <- "The certificate's record of removed data was altered."
      }
    }
  }
  if (!length(bad_columns) || !length(bad_rows)) problem <- "The certificate's record was altered."
  o <- order(at_row, at_column, method = "radix")

  details$modified_cells <- data.frame(row = at_row[o], column = at_column[o], stringsAsFactors = FALSE)
  details$suspect <- length(bad_rows) > 1L && length(bad_columns) > 1L
  verification_result("modified", manifest, details, problem)
}

verification_result <- function(status, manifest = NULL, details = list(), problems = character()) {
  list(
    status = status,
    manifest = manifest,
    removed_columns = details$removed_columns %||% character(),
    removed_rows = details$removed_rows %||% 0L,
    added_columns = details$added_columns %||% character(),
    modified_cells = details$modified_cells %||% data.frame(row = integer(), column = character(), stringsAsFactors = FALSE),
    suspect = details$suspect %||% FALSE,
    problems = as.character(problems)
  )
}

as_verification <- function(result, data, certificate) {
  m <- result$manifest
  structure(list(
    status = result$status,
    valid = identical(result$status, "valid"),
    kind = if (is_whole_file(m)) "file" else "cells",
    removed_columns = result$removed_columns,
    removed_rows = result$removed_rows,
    added_columns = result$added_columns,
    modified_cells = result$modified_cells,
    suspect = result$suspect,
    problems = result$problems,
    certificate = text_or_na(field(m, "id")),
    issuer = field(m, "issuer"),
    organization = text_or_na(field(m, "organization")),
    survey = field(m, "survey"),
    issued_at = utc_time(field(m, "issued_at")),
    manifest = m,
    files = c(data = data, certificate = certificate)
  ), class = "sondavi_verification")
}

# -- leaving columns and rows out ----------------------------------------------------------

# The new record, as the platform's redactor writes it: a dropped column keeps one leaf per
# original row; a column that keeps a seed loses it as soon as any row is missing - the seed
# would let anyone recompute the salt of a removed row and guess its value - and gets one salt
# per present row and one leaf per removed row instead.
redact_record <- function(cert, manifest, table, drop_columns, drop_rows) {
  n <- as_count(field(manifest, "rows"))
  header <- table$header
  cells <- table$cells
  original <- unlist(field(cert$proof, "rows")) %||% integer()
  file_row <- rep(NA_integer_, n)
  file_row[original + 1L] <- seq_along(original)
  present <- !is.na(file_row)
  dropped <- rep(FALSE, n)
  dropped[original[drop_rows] + 1L] <- TRUE
  complete_after <- length(original) - sum(dropped) == n

  columns <- lapply(field(cert$proof, "columns"), function(entry) {
    name <- field(entry, "name")
    if (php_truthy(field(entry, "removed"))) return(entry)
    values <- character(n)
    values[present] <- cells[file_row[present], match(name, header)]
    seed <- field(entry, "seed")
    recorded <- if (is.null(seed)) vapply(field(entry, "salts"), identity, "") else character(n)
    salts <- if (is.null(seed)) {
      lapply(seq_len(n), function(i) if (present[i]) hex_raw(recorded[i], 16L) else NULL)
    } else {
      key <- hex_raw(seed, 32L)
      lapply(seq_len(n) - 1L, function(i) salt_from_seed(key, i))
    }
    leaf_hex <- function(rows) {
      vapply(column_leaves(name, salts[rows], values[rows]), raw_hex, "")
    }

    if (name %in% drop_columns) {
      # A row removed earlier has only its leaf left ("-" + leaf), and keeps it.
      out <- substring(recorded, 2)
      if (any(present)) out[present] <- leaf_hex(present)
      return(list(name = name, removed = TRUE, leaves = as.list(out)))
    }
    if (!is.null(seed) && complete_after) return(entry)
    out <- recorded                                        # a row removed earlier: already a leaf
    if (any(dropped)) out[dropped] <- paste0("-", leaf_hex(dropped))
    kept <- present & !dropped
    if (any(kept)) out[kept] <- vapply(salts[kept], raw_hex, "")
    list(name = name, salts = as.list(out))
  })

  kept_rows <- which(!dropped[original + 1L])
  kept_columns <- which(!header %in% drop_columns)
  new_cert <- cert
  new_cert[["proof"]] <- list(rows = as.list(original[kept_rows]), columns = columns)
  list(
    header = header[kept_columns],
    cells = cells[kept_rows, kept_columns, drop = FALSE],
    certificate = new_cert
  )
}

# -- the hashing (format version 1) -----------------------------------------------------------

u32 <- function(n) writeBin(as.integer(n), raw(), size = 4, endian = "big")

sha256_raw <- function(x, key = NULL) unclass(openssl::sha256(x, key = key))

# The salt of original row i in a column with seed `seed`: the first 16 bytes of
# HMAC-SHA-256(seed, u32(i)).
salt_from_seed <- function(seed, i) sha256_raw(u32(i), key = seed)[1:16]

# leaf = SHA-256(salt || u32(bytes of column) || column || u32(bytes of value) || value), UTF-8.
column_leaves <- function(name, salts, values) {
  column <- charToRaw(enc2utf8(name))
  prefix <- c(u32(length(column)), column)
  lapply(seq_along(values), function(k) {
    value <- charToRaw(enc2utf8(values[k]))
    sha256_raw(c(salts[[k]], prefix, u32(length(value)), value))
  })
}

# Over all original rows, in their original order.
column_root <- function(name, leaves) {
  column <- charToRaw(enc2utf8(name))
  sha256_raw(c(charToRaw("sondavi-col"), u32(length(column)), column, unlist(leaves)))
}

# Over all original columns, in their original order.
row_root <- function(i, leaves) {
  sha256_raw(c(charToRaw("sondavi-row"), u32(i), unlist(leaves)))
}

raw_hex <- function(x) paste(as.character(x), collapse = "")

# Hex of exactly `bytes` bytes, or the record cannot be read - as on the platform: a salt is
# 16 bytes, a seed and a leaf 32.
is_hex <- function(x, bytes) {
  is_string(x) && nchar(x, "bytes") == 2L * bytes && !grepl("[^0-9a-fA-F]", x)
}

hex_raw <- function(x, bytes) {
  if (!is_hex(x, bytes)) stop("not ", bytes, " bytes of hexadecimal")
  at <- seq(1L, 2L * bytes, by = 2L)
  as.raw(strtoi(substring(x, at, at + 1L), 16L))
}

# Base64 as strictly as the platform reads it: nothing outside the alphabet.
strict_base64 <- function(x) {
  x <- gsub("[ \t\r\n]", "", x)
  if (!grepl("^[A-Za-z0-9+/]*={0,2}$", x) || nchar(x) %% 4L == 1L) return(NULL)
  x <- sub("=+$", "", x)
  x <- paste0(x, strrep("=", (4L - nchar(x) %% 4L) %% 4L))
  tryCatch(openssl::base64_decode(x), error = function(e) NULL)
}

signature_ok <- function(message, signature, key) {
  isTRUE(tryCatch(
    openssl::ed25519_verify(message, signature, openssl::read_ed25519_pubkey(key)),
    error = function(e) FALSE
  ))
}

# -- the issuer's keys -------------------------------------------------------------------------

# The public key with a given id, for one check (and the second check of a redaction), or NULL.
# It is looked up only when needed: a certificate from an untrusted issuer never makes this
# package contact that issuer.
#
# Keys given with `keys =` are used as they are and nothing else. Otherwise the keys of
# https://sondavi.io built into the package come first, and only for a key that is not among
# them the issuer's published keys are fetched - once per issuer.
key_ring <- function(keys) {
  given <- NULL
  cache <- list()
  function(issuer, id) {
    if (!is.null(keys)) {
      if (is.null(given)) given <<- keys_by_id(keys_document(keys))
      return(field(given, id))
    }
    slot <- paste0("issuer:", issuer)
    known <- field(cache, slot) %||% list(ring = builtin_ring(issuer), fetched = FALSE)
    if (is.null(field(known$ring, id)) && !known$fetched) {
      fetched <- keys_by_id(fetch_json(paste0(issuer, KEYS_PATH), "the issuer's public keys", hint = paste0(
        "\nThe keys of https://sondavi.io known when this version of the package was released are ",
        "built in; a newer key, or a key of another issuer, needs a connection to the issuer. ",
        "Or pass the keys yourself with `keys =` (a path, a URL or the document)."
      )))
      known$ring <- c(known$ring, fetched[setdiff(names(fetched), names(known$ring))])
      known$fetched <- TRUE
    }
    cache[[slot]] <<- known
    field(known$ring, id)
  }
}

# The keys built into the package (inst/certificate-keys.json: a saved copy of
# https://sondavi.io/.well-known/sondavi-certificate-keys.json), for that issuer only. A missing
# or unreadable file is no keys, not an error.
builtin_ring <- function(issuer) {
  document <- tryCatch(builtin_key_document(), error = function(e) NULL)
  url <- trim_slash(text_or_empty(field(field(document, "issuer"), "url")))
  if (!nzchar(url) || !identical(url, issuer)) return(list())
  tryCatch(keys_by_id(document), error = function(e) list())
}

builtin_key_document <- function() {
  path <- system.file("certificate-keys.json", package = "sondavi")
  if (!nzchar(path)) return(NULL)
  tryCatch(jsonlite::parse_json(read_utf8(path), simplifyVector = FALSE), error = function(e) NULL)
}

keys_document <- function(keys) {
  if (is_string(keys) && grepl("^https?://", keys, ignore.case = TRUE)) {
    return(fetch_json(keys, "the keys document"))
  }
  if (is_string(keys)) {
    if (!file.exists(keys)) stop("No keys document at ", keys, ".", call. = FALSE)
    document <- tryCatch(jsonlite::parse_json(read_utf8(keys), simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(document)) stop(keys, " is not a keys document (not JSON).", call. = FALSE)
    return(document)
  }
  if (is.list(keys)) return(keys)
  stop("`keys` must be NULL, the path or URL of a keys document, or the document itself.", call. = FALSE)
}

# Only a 32-byte key whose id is its own ("ed25519:" + the first 16 hex digits of SHA-256 over
# it) is taken; any other entry is no key, so a certificate naming it gives "unknown_key".
keys_by_id <- function(document) {
  entries <- if (is.data.frame(document)) {
    document
  } else if (is.list(document) && "keys" %in% names(document)) {
    document[["keys"]]
  } else if (is_array(document)) {
    document
  } else {
    stop("This is not a keys document: it has no list of keys. Is it the issuer's ",
         "sondavi-certificate-keys.json?", call. = FALSE)
  }
  if (is.data.frame(entries)) {
    entries <- lapply(seq_len(nrow(entries)), function(i) as.list(entries[i, , drop = FALSE]))
  }
  ring <- list()
  for (entry in entries) {
    id <- field(entry, "id")
    key <- field(entry, "public_key")
    if (!is_string(id) || !is_string(key)) next
    key <- tryCatch(openssl::base64_decode(key), error = function(e) raw(0))
    if (length(key) == 32L && identical(id, key_id(key))) ring[[id]] <- key
  }
  ring
}

key_id <- function(public_key) paste0("ed25519:", substr(raw_hex(sha256_raw(public_key)), 1, 16))

fetch_json <- function(url, what, hint = "") {
  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, sondavi_user_agent())
  req <- httr2::req_headers(req, Accept = "application/json")
  req <- httr2::req_timeout(req, 30)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    stop("Could not fetch ", what, " from ", url, ": ", conditionMessage(resp), hint, call. = FALSE)
  }
  if (httr2::resp_status(resp) != 200L) {
    stop("Could not fetch ", what, " from ", url, ": the server answered ",
         httr2::resp_status(resp), ".", hint, call. = FALSE)
  }
  document <- tryCatch(jsonlite::parse_json(httr2::resp_body_string(resp), simplifyVector = FALSE),
                       error = function(e) NULL)
  if (is.null(document)) stop(url, " did not answer with JSON.", hint, call. = FALSE)
  document
}

# -- files -------------------------------------------------------------------------------------

check_input_file <- function(path, what) {
  if (!is_string(path)) {
    stop("`", what, "` must be the path of a file. A data frame will not do: it has already ",
         "converted the text the certificate is about.", call. = FALSE)
  }
  if (!file.exists(path) || dir.exists(path)) stop("No such file: ", path, call. = FALSE)
}

check_trust <- function(trust) {
  if (!is.character(trust) || anyNA(trust)) {
    stop("`trust` must be the addresses of the issuers you accept, e.g. \"https://sondavi.io\".", call. = FALSE)
  }
  trust
}

read_bytes <- function(path) {
  size <- file.size(path)
  if (is.na(size) || size == 0) return(raw(0))
  bytes <- readBin(path, "raw", size)
  if (length(bytes) >= 3L && identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) bytes <- bytes[-(1:3)]
  bytes
}

read_utf8 <- function(path) {
  text <- rawToChar(read_bytes(path))
  Encoding(text) <- "UTF-8"
  text
}

# NULL when the file is no JSON object at all: a "malformed" certificate, not an error.
read_certificate <- function(path) {
  cert <- tryCatch(jsonlite::parse_json(read_utf8(path), simplifyVector = FALSE), error = function(e) NULL)
  if (is.list(cert)) cert else NULL
}

write_certificate <- function(path, cert) {
  json <- jsonlite::toJSON(cert, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  writeBin(c(charToRaw(enc2utf8(as.character(json))), charToRaw("\n")), path)
}

# The CSV of a certified dataset, read strictly - exactly as the platform's, the browser's and the
# Python verifier read it (shared cases: tests/fixtures/certificates/csv-cases.json). RFC 4180
# with nothing tolerated that another program could read differently: valid UTF-8, one leading
# byte order mark removed (a second one is part of the first name), no NUL, records end with LF or
# CRLF. A quoted field starts and ends at the field's edges ("" inside is one quote, a line break
# inside stays what it was: "\r\n" remains "\r\n"); a lone CR outside quotes, a quote inside an
# unquoted field, text after a closing quote, an unterminated quote and a control character in a
# header name make the file unreadable. Empty lines do not count, before the header either -
# except in a file whose header has one column, where an empty line is one empty cell; a line
# `""` is a row with one empty field. utils::read.csv() cannot be made to do any of that, so this
# reads the whole file as one string, one field per match.
#
# Returns the header, the data rows as read, the width of each, and a character matrix with one
# row per data row and one column per header name (the check refuses any row whose width is not
# the header's).
NOT_WELL_FORMED <- "The data file is not a well-formed CSV file."

read_csv_table <- function(path) {
  parse_csv_bytes(read_bytes(path))   # read_bytes() removes one leading byte order mark
}

parse_csv_bytes <- function(bytes) {
  unreadable <- function() stop(structure(class = c("sondavi_csv_error", "error", "condition"),
                                          list(message = NOT_WELL_FORMED, call = NULL)))
  if (any(bytes == as.raw(0))) unreadable()
  records <- list()
  blank <- logical()
  if (length(bytes)) {
    text <- rawToChar(bytes)
    if (!validUTF8(text)) unreadable()
    Encoding(text) <- "bytes"
    # A quoted field, or a plain one without quote, comma or line break; then what ends it.
    pattern <- '(?:"((?:[^"]++|"")*+)"|([^",\r\n]*+))(,|\r\n|\n|\\z)'
    m <- gregexpr(pattern, text, perl = TRUE, useBytes = TRUE)[[1]]
    start <- as.integer(m)
    end <- start + attr(m, "match.length")
    # Every byte must belong to a field: a gap is a lone CR, a stray or unterminated quote, or text
    # after a closing quote.
    if (start[1] != 1L || any(start[-1] != end[-length(end)]) || end[length(end)] != length(bytes) + 1L) {
      unreadable()
    }
    from <- attr(m, "capture.start")
    len <- attr(m, "capture.length")
    group <- function(k) {
      out <- substring(text, from[, k], from[, k] + len[, k] - 1L)
      out[len[, k] <= 0L] <- ""
      out
    }
    quoted <- from[, 1] > 0L
    ends_with <- group(3)
    value <- group(2)
    value[quoted] <- gsub('""', '"', group(1)[quoted], fixed = TRUE, useBytes = TRUE)
    # A comma as the very last byte leaves one more, empty field.
    if (ends_with[length(ends_with)] == ",") {
      value <- c(value, "")
      quoted <- c(quoted, FALSE)
      ends_with <- c(ends_with, "")
    }
    Encoding(value) <- "UTF-8"
    record <- cumsum(c(1L, ends_with[-length(ends_with)] != ","))
    records <- unname(split(value, record))
    first_quoted <- quoted[!duplicated(record)]
    blank <- lengths(records) == 1L & !first_quoted & vapply(records, function(r) !nzchar(r[1]), NA)
  }

  # An empty line is no row - except in a one-column file, where it is one empty cell.
  # Empty lines before the header do not count.
  first <- which(!blank)[1]
  if (is.na(first)) return(list(header = character(), rows = list(), widths = integer(), cells = matrix(character(), 0, 0)))
  header <- records[[first]]
  if (any(grepl("[\\x{01}-\\x{1f}\\x{7f}]", header, perl = TRUE, useBytes = TRUE))) unreadable()
  rows <- records[-seq_len(first)]
  if (length(header) != 1L) rows <- rows[!blank[-seq_len(first)]]
  width <- length(header)
  cells <- if (!length(rows)) {
    matrix(character(), 0, width)
  } else if (all(lengths(rows) == width)) {
    matrix(unlist(rows, use.names = FALSE), ncol = width, byrow = TRUE)
  } else {
    matrix(unlist(lapply(rows, function(r) {
      r <- r[seq_len(min(length(r), width))]
      c(r, rep("", width - length(r)))
    }), use.names = FALSE), ncol = width, byrow = TRUE)
  }
  list(header = header, rows = rows, widths = lengths(rows), cells = cells)
}

# As the platform writes it: a byte order mark, comma, "\n" after each row, and a field quoted
# (quotes doubled) when it holds a comma, a quote, a line break, a tab or a space.
write_csv_table <- function(path, header, cells) {
  field_text <- function(x) {
    quote <- grepl("[,\"\r\n\t ]", x, useBytes = TRUE)
    x[quote] <- paste0("\"", gsub("\"", "\"\"", x[quote], fixed = TRUE, useBytes = TRUE), "\"")
    x
  }
  lines <- paste(field_text(header), collapse = ",")
  if (nrow(cells)) {
    quoted <- matrix(field_text(as.vector(cells)), nrow = nrow(cells))
    lines <- c(lines, do.call(paste, c(lapply(seq_len(ncol(quoted)), function(j) quoted[, j]), sep = ",")))
  }
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(paste0(lines, "\n", collapse = ""))), path)
}

# -- small helpers -----------------------------------------------------------------------------

# An element of a parsed JSON object by its exact name; NULL when absent. `$` would match a
# prefix, and on a certificate that is a hole.
field <- function(x, name) {
  if (is.list(x) && !is.null(names(x)) && name %in% names(x)) x[[name]] else NULL
}

is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

text_or_empty <- function(x) if (is_string(x)) x else ""

text_or_na <- function(x) if (is_string(x)) x else NA_character_

# A JSON array (an object would be a named list).
is_array <- function(x) is.list(x) && is.null(names(x))

as_array <- function(x) if (is_array(x)) x else stop("not an array")

as_count <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) stop("not a count")
  as.integer(trunc(x))
}

trim_slash <- function(x) sub("/+$", "", x)

# A scalar as the platform's `(string)` cast writes it; anything else cannot start with "-".
php_string <- function(x) {
  if (is_string(x)) return(x)
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) return(format(x, scientific = FALSE, trim = TRUE))
  ""
}

# What the platform's `! empty(...)` accepts as set.
php_truthy <- function(x) {
  if (is.null(x) || length(x) == 0L) return(FALSE)
  if (is.list(x)) return(TRUE)
  if (length(x) != 1L || is.na(x)) return(FALSE)
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  if (is.character(x)) return(nzchar(x) && x != "0")
  FALSE
}

utc_time <- function(x) {
  if (!is_string(x)) return(as.POSIXct(NA, tz = "UTC"))
  as.POSIXct(sub("Z$", "", x), tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS")
}

# -- printing ----------------------------------------------------------------------------------

verification_lines <- function(x) {
  issuer_url <- text_or_empty(field(x$issuer, "url"))
  whole_file <- identical(x$kind, "file")
  headline <- switch(x$status,
    valid = if (whole_file) {
      "Valid: this is the file that was certified."
    } else {
      "Valid: every certified cell matches."
    },
    modified = if (whole_file) {
      paste("Modified: this is not the file that was certified - its fingerprint differs.",
            "Any change counts, even saving it again.")
    } else if (nrow(x$modified_cells)) {
      sprintf("Modified: the certificate is genuine, but %d %s from what was certified.",
              nrow(x$modified_cells), if (nrow(x$modified_cells) == 1L) "cell differs" else "cells differ")
    } else {
      "Modified: the certificate is genuine, but the data or its record was altered."
    },
    unverifiable = "Unverifiable: the file does not have the structure the certificate describes.",
    invalid_signature = "Invalid signature: the certificate was altered after it was issued.",
    untrusted_issuer = sprintf(paste(
      "Untrusted issuer: the certificate names %s as its issuer, which is not in `trust`.",
      "Nothing was checked. Add it to `trust` only if you trust that installation."
    ), if (nzchar(issuer_url)) issuer_url else "no address"),
    unknown_key = sprintf("Unknown key: the published keys of %s do not include the key that signed this certificate (%s).",
                          issuer_url, text_or_empty(field(x$manifest, "key_id"))),
    malformed = "Malformed: this is not a certificate this version of the package can read.",
    paste0("Status: ", x$status)
  )
  out <- headline

  if (identical(x$status, "modified") && nrow(x$modified_cells)) {
    shown <- utils::head(x$modified_cells, 20L)
    out <- c(out, sprintf("  row %d, column %s", shown$row, shown$column))
    if (nrow(x$modified_cells) > 20L) {
      out <- c(out, sprintf("  ... and %d more (see $modified_cells)", nrow(x$modified_cells) - 20L))
    }
    if (isTRUE(x$suspect)) {
      out <- c(out, paste(
        "Cells differ in several rows and several columns. The certificate holds one sum per row",
        "and one per column, not each cell, so these are suspects: the cells where a differing",
        "row meets a differing column. Not all of them need to have changed."
      ))
    }
  }
  if (length(x$problems) && !(whole_file && identical(x$status, "modified"))) out <- c(out, x$problems)

  # Who issued it, for which study, is only established once the signature has been checked.
  if (x$status %in% c("valid", "modified", "unverifiable")) {
    s <- x$survey
    survey <- text_or_empty(field(s, "title"))
    about <- c(
      if (!is.null(field(s, "id"))) paste("id", field(s, "id")),
      if (!is.null(field(s, "version"))) paste("questionnaire version", field(s, "version")),
      if (is_string(field(s, "privacy"))) field(s, "privacy")
    )
    if (length(about)) survey <- sprintf("%s (%s)", survey, paste(about, collapse = ", "))
    issuer <- text_or_empty(field(x$issuer, "name"))
    if (nzchar(issuer_url)) issuer <- if (nzchar(issuer)) sprintf("%s, %s", issuer, issuer_url) else issuer_url
    responses <- field(x$manifest, "responses")
    period <- c(utc_time(field(responses, "first")), utc_time(field(responses, "last")))
    rows <- as_count_or_na(field(x$manifest, "rows"))
    out <- c(out,
      label_line("Survey:", survey),
      label_line("Organisation:", text_or_empty(x$organization)),
      label_line("Issuer:", issuer),
      label_line("Issued:", format_utc(x$issued_at)),
      if (!anyNA(period)) label_line("Responses:", paste(format_utc(period[1]), "to", format_utc(period[2]))),
      label_line("Certificate:", text_or_empty(x$certificate))
    )
    if (whole_file) {
      if (identical(x$status, "valid")) out <- c(out, file_lines(x))
    } else if (x$status != "unverifiable") {
      out <- c(out,
        if (!is.na(rows)) label_line("Rows:", sprintf("%d certified, %d in this file", rows, rows - x$removed_rows)),
        if (x$removed_rows > 0) label_line("Removed rows:", x$removed_rows),
        column_lines(x)
      )
    }
  }
  out
}

# What the platform's comparison of a certified file found, as the signed manifest records it.
file_lines <- function(x) {
  basis <- field(x$manifest, "basis")
  missing <- field(basis, "missing")
  from <- field(basis, "derived_from")
  type <- text_or_empty(field(field(field(basis, "file"), "format"), "type"))
  own <- switch(type, csv = "CSV file", xlsx = "Excel file", json = "JSON file", "file")
  total <- as_count_or_na(field(basis, "responses_total"))
  in_file <- as_count_or_na(field(basis, "responses_in_file"))
  by_status <- counts_text(field(missing, "by_status"),
                           c(complete = "complete", partial = "incomplete", screened_out = "screened out",
                             unknown = "of unknown status"))
  by_group <- counts_text(field(missing, "by_group"))
  no_group <- as_count_or_na(field(missing, "group_unknown"))
  if (!is.na(no_group) && no_group > 0) {
    by_group <- c(by_group, sprintf("%s%d of a group no file names", if (length(by_group)) "and " else "", no_group))
  }
  written <- counts_text(field(basis, "written_differently"), cells = TRUE)
  order <- text_or_empty(field(basis, "row_order"))
  matched <- text_or_empty(field(basis, "matched_by"))
  c(
    label_line("How:", sprintf("Certified from the researcher's own %s, after every value was compared with %s",
                               own, if (is.list(from)) "a file certified earlier" else "the stored responses")),
    if (is.list(from)) label_line("Derived from:", derived_text(from)),
    label_line("File:", file_text(field(basis, "file"))),
    label_line("Read as:", read_as(field(field(basis, "file"), "format"))),
    if (nzchar(matched)) label_line("Rows matched:", if (identical(matched, "response_id")) "by response ID" else
      "by their content (the file has no response ID)"),
    if (!is.na(in_file) && !is.na(total)) label_line("Rows:", sprintf("%d of %d responses of this survey", in_file, total)),
    if (x$removed_rows > 0) label_line("Left out:", paste0(
      x$removed_rows, if (x$removed_rows == 1L) " response" else " responses",
      if (length(by_status)) paste0(": ", paste(by_status, collapse = ", "))
    )),
    if (length(by_group)) label_line("  by group:", paste(by_group, collapse = ", ")),
    column_lines(x),
    if (length(written)) label_line("Notation:", paste(paste(written, collapse = ", "), "- the same value in another notation")),
    if (nzchar(order)) label_line("Row order:", switch(order, unchanged = "as in the survey's data", changed = "changed", order))
  )
}

# The file's columns as the platform's /verify page sorts them, each in one place (the rule of
# resources/js/certificates/columns.js): certified; in the file but not certified - left out when the
# certificate was issued and back in this copy, computed by the platform, or added; missing from the
# file - left out when the certificate was issued, or (a certified file) in the survey's data but not
# in the file; and the certified columns the participant's browser sent.
column_groups <- function(x) {
  basis <- field(x$manifest, "basis")
  computed <- names_in(field(basis, "columns_derived"))
  removed <- x$removed_columns
  added <- x$added_columns
  if (identical(field(basis, "kind"), "file")) {
    certified <- names_in(field(basis, "columns_checked"))
    groups <- list(certified = certified, left_out = character(), computed = computed,
                   added = added[!added %in% computed], missing_left_out = character(),
                   missing_not_in_file = removed)
  } else {
    columns <- field(x$manifest, "columns")
    named <- if (is_array(columns)) vapply(columns, function(col) text_or_empty(field(col, "name")), "") else character()
    certified <- named[!named %in% removed]
    groups <- list(certified = certified,
                   left_out = added[added %in% removed & !added %in% computed],
                   computed = added[added %in% computed],
                   added = added[!added %in% removed & !added %in% computed],
                   missing_left_out = removed[!removed %in% added & !removed %in% computed],
                   missing_not_in_file = character())
  }
  browser <- names_in(field(basis, "browser_reported"))
  groups$browser <- browser[browser %in% certified]
  groups
}

# Certified, in the file but not certified, missing from the file - in the verify page's words.
column_lines <- function(x) {
  g <- column_groups(x)
  whole_file <- identical(x$kind, "file")
  offline <- as_count_or_na(field(field(x$manifest, "basis"), "offline_synced"))
  reasons <- function(label, parts) {
    parts <- Filter(function(p) length(p$names) > 0L, parts)
    if (!length(parts)) return(label_line(label, "none"))
    n <- sum(vapply(parts, function(p) length(p$names), 0L))
    c(label_line(label, sprintf("%d %s", n, if (n == 1L) "column" else "columns")),
      vapply(parts, function(p) note_line(sprintf("%s: %s", p$why, paste(p$names, collapse = ", "))), ""))
  }
  c(
    label_line("Certified:", sprintf("%d %s, %s", length(g$certified), if (length(g$certified) == 1L) "column" else "columns",
      if (whole_file) "every value compared with what participants entered" else "every value as participants entered it")),
    reasons("Not certified:", list(
      list(names = g$left_out, why = "left out when the certificate was issued, so it cannot check them"),
      list(names = g$computed, why = "worked out by the platform when the file was made, never certified"),
      list(names = g$added, why = if (whole_file) "not part of the survey's data, added by the researcher" else
        "added after the certificate was issued")
    )),
    reasons("Missing:", list(
      list(names = g$missing_left_out, why = "left out when the certificate was issued"),
      list(names = g$missing_not_in_file, why = "in the survey's data, not in this file")
    )),
    if (length(g$browser)) c(
      label_line("Sent by browser:", paste(g$browser, collapse = ", ")),
      note_line("certified as stored, but sent by the participant's browser rather than measured by the platform")
    ),
    if (!is.na(offline) && offline > 0) label_line("From offline devices:", sprintf(
      "%d %s collected offline; the time they were completed is the device's", offline,
      if (offline == 1L) "response" else "responses"
    ))
  )
}

# How the platform read the researcher's file - a reviewer's program should read it the same way.
read_as <- function(format) {
  type <- text_or_empty(field(format, "type"))
  if (identical(type, "xlsx")) return(sprintf("Excel, sheet \"%s\"", text_or_empty(field(format, "sheet"))))
  if (identical(type, "json")) return("JSON, one object per response")
  if (identical(type, "csv")) {
    delimiter <- text_or_empty(field(format, "delimiter"))
    separated <- switch(delimiter, "," = "commas", ";" = "semicolons", tab = "tabs", delimiter)
    encoding <- field(format, "encoding")
    return(paste(c(sprintf("CSV, values separated by %s", separated), if (is_string(encoding)) encoding), collapse = ", "))
  }
  if (nzchar(type)) type else "unknown"
}

# {"partial": 2, "screened_out": 0} -> "2 incomplete"; a key without a label is named as it is.
# With `cells`, {"q_num": 1} -> "q_num (1 cell)".
counts_text <- function(counts, labels = NULL, cells = FALSE) {
  if (!is.list(counts) || is.null(names(counts))) return(character())
  out <- character()
  for (key in names(counts)) {
    n <- counts[[key]]
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n <= 0) next
    out <- c(out, if (cells) {
      sprintf("%s (%d %s)", key, as.integer(n), if (n == 1) "cell" else "cells")
    } else if (is.null(labels)) {
      sprintf("%s: %d", key, as.integer(n))
    } else {
      sprintf("%d %s", as.integer(n), if (key %in% names(labels)) labels[[key]] else key)
    })
  }
  out
}

# "certificate 00000000 of 2026-10-20 (dataset.csv)"
derived_text <- function(from) {
  issued <- utc_time(field(from, "issued_at"))
  day <- if (is.na(issued)) text_or_empty(field(from, "issued_at")) else format(issued, "%Y-%m-%d", tz = "UTC")
  file <- text_or_empty(field(from, "file"))
  sprintf("certificate %s of %s%s", substr(text_or_empty(field(from, "certificate")), 1, 8), day,
          if (nzchar(file)) sprintf(" (%s)", file) else "")
}

# "dataset.csv (365 bytes)"
file_text <- function(file) {
  bytes <- field(file, "bytes")
  size <- if (is.numeric(bytes) && length(bytes) == 1L) sprintf("%s bytes", format(bytes, big.mark = ",", scientific = FALSE))
  name <- text_or_empty(field(file, "name"))
  if (nzchar(name) && length(size)) sprintf("%s (%s)", name, size) else paste0(name, size)
}

# Labels in one column; a longer label is followed by a single space.
label_line <- function(label, value) sprintf("%-16s %s", label, value)

note_line <- function(text) paste0(strrep(" ", 17), text)

format_utc <- function(x) if (is.na(x)) "unknown" else format(x, "%Y-%m-%d %H:%M UTC", tz = "UTC")

as_count_or_na <- function(x) tryCatch(as_count(x), error = function(e) NA_integer_)
