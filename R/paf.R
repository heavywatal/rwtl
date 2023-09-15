#' Unify pairwise alignment files to Pairwise mApping Format (PAF)
#'
#' PAF is described in [minimap2 manual](https://lh3.github.io/minimap2/minimap2.html).
#' @param x data.frame.
#' @rdname paf
#' @export
as_paf = function(x) UseMethod("as_paf")

#' @export
as_paf.default = function(x) {
  res = x |>
    dplyr::select(dplyr::all_of(paf_columns)) |>
    dplyr::mutate(chr = as_factor_numeric(.data$chr), qchr = as_factor_numeric(.data$qchr))
  class(res) = paf_class
  res
}

paf_columns = c(
  "qchr", "qsize", "qstart", "qend", "strand", "chr", "size", "start", "end", "match", "width", "score"
)

#' @rdname paf
#' @export
read_paf = function(file) {
  readr::read_tsv(file, col_names = paf_columns, col_types = "ciiicciiiiii") |>
    as_paf()
}

#' @rdname paf
#' @export
write_paf = function(x, file) {
  readr::write_tsv(x, file, na = "", col_names = FALSE)
}

#' @param file paths to alignment files.
#' @seealso <https://genome.ucsc.edu/goldenPath/help/axt.html>
#' @rdname paf
#' @export
read_axt = function(file) {
  # The first base is numbered 1.
  # The end base is included.
  # If the strand value is "-", the values of the aligning organism's start and end
  # fields are relative to the reverse-complemented coordinates of its chromosome.
  if (length(file) > 1L) {
    res = purrr::map(file, read_axt) |> purrr::list_rbind()
  } else {
    lines = readr::read_lines(file, skip_empty_rows = TRUE) |>
      stringr::str_subset("^\\d")
    res = readr::read_delim(
      I(lines),
      delim = " ",
      col_names = c("id", "chr", "start", "end", "qchr", "qstart", "qend", "strand", "score"),
      col_types = "iciiciici",
      guess_max = 0L
    )
    .nrow = nrow(res)
    stopifnot(res[["id"]][.nrow] == .nrow - 1L)
  }
  class(res) = c("tbl_axt", class(res))
  res
}

#' @export
as_paf.tbl_axt = function(x) {
  x |>
    dplyr::mutate(start = .data$start - 1L, qstart = .data$qstart - 1L) |>
    dplyr::mutate(qsize = NA_integer_, size = NA_integer_) |>
    dplyr::mutate(width = .data$qend - .data$qstart, match = .data$width) |>
    as_paf.default()
}

#' @seealso <https://genome.ucsc.edu/goldenPath/help/chain.html>
#' @rdname paf
#' @export
read_chain = function(file) {
  # The positions are represented as zero-based half-open intervals.
  # e.g., the first 100 bases is represented as {start: 0, end: 100}
  if (length(file) > 1L) {
    res = purrr::map(file, read_chain) |> purrr::list_rbind()
  } else {
    lines = readr::read_lines(file, skip_empty_rows = TRUE) |>
      stringr::str_subset("^chain")
    res = readr::read_delim(
      I(lines),
      delim = " ",
      col_names = c(
        "score", "chr", "size", "tstrand", "start", "end",
        "qchr", "qsize", "strand", "qstart", "qend", "id"
      ),
      col_types = "_iciciiciciii",
      guess_max = 0L
    )
    .nrow = nrow(res)
    stopifnot(res[["tstrand"]] == "+")
    stopifnot(res[["start"]] < res[["end"]])
    stopifnot(res[["qstart"]] < res[["qend"]])
  }
  class(res) = c("tbl_chain", class(res))
  res
}

#' @export
as_paf.tbl_chain = function(x) {
  x |>
    dplyr::mutate(width = .data$qend - .data$qstart, match = .data$width) |>
    as_paf.default()
}

#' @seealso <https://genome.ucsc.edu/goldenPath/help/net.html>
#' @rdname paf
#' @export
read_net = function(file) {
  if (length(file) > 1L) {
    res = purrr::map(file, read_net) |> purrr::list_rbind()
  } else {
    lines = readr::read_lines(file, skip_empty_rows = TRUE) |>
      stringr::str_subset("^net|^ fill")
    header = stringr::str_split_1(lines[1L], " ")
    target_chr = header[2L]
    target_size = header[3L]
    lines = stringr::str_remove(lines[-1L], "^ fill ")
    res = readr::read_delim(
      I(lines),
      delim = " ",
      col_names = c(
        "start", "twidth", "qchr", "strand", "qstart", "width",
        "_1", "id", "_2", "score", "_3", "ali", "_4", "qdup", "_5", "type"
      ),
      col_types = "iiccii_i_i_i_i_c",
      guess_max = 0L
    ) |>
      dplyr::mutate(chr = target_chr, size = target_size)
  }
  class(res) = c("tbl_net", class(res))
  res
}

#' @export
as_paf.tbl_net = function(x) {
  x |>
    dplyr::mutate(end = .data$start + .data$twidth, qend = .data$qstart + .data$width) |>
    dplyr::mutate(match = .data$ali, qsize = NA_integer_) |>
    as_paf.default()
}

#' @seealso <https://genome.ucsc.edu/FAQ/FAQformat.html#format5>
#' @rdname paf
#' @export
read_singmaf = function(file) {
  if (length(file) > 1L) {
    res = purrr::map(file, read_singmaf) |> purrr::list_rbind()
  } else {
    lines = readr::read_lines(file, skip_empty_rows = TRUE)
    scores = lines |>
      stringr::str_subset("^a ") |>
      readr::parse_number()
    num_blocks = length(scores)
    lines = lines |> stringr::str_subset("^s ")
    raw = readr::read_table(
      I(lines),
      col_names = c("src", "start", "width", "strand", "size"),
      col_types = "_ciici_",
      guess_max = 0L
    )
    long = raw |>
      tidyr::separate_wider_delim("src", ".", names = c("species", "chr")) |>
      dplyr::mutate(end = .data$start + .data$width, .after = .data$start)
    res = long |>
      dplyr::select(!"species") |>
      dplyr::mutate(id = rep(seq_len(num_blocks), each = 2L)) |>
      dplyr::mutate(prefix = rep(c("", "q"), num_blocks)) |>
      tidyr::pivot_wider(
        values_from = !c("prefix", "id"),
        names_from = "prefix",
        names_glue = "{prefix}{.value}",
        names_vary = "slowest"
      ) |>
      dplyr::select(!c("id", "strand")) |>
      dplyr::rename(strand = .data$qstrand) |>
      dplyr::mutate(score = scores)
  }
  class(res) = c("tbl_singmaf", class(res))
  res
}

#' @export
as_paf.tbl_singmaf = function(x) {
  x |>
    dplyr::mutate(match = pmin(.data$width, .data$qwidth), width = .data$qwidth) |>
    as_paf.default()
}

paf_class = c("paf", "tbl_df", "tbl", "data.frame")
