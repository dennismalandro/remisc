#' Miscellaneous Helper Functions
#'
#' Calculate the empirical cumulative distribution function.
#'
#' @param x Numeric vector
#' @param adjust Logical.  Adjust so that \code{F(max(x)) < 1}?
#' @export
#' @examples
#' edf(rnorm(9))

edf <- function(x, adjust = TRUE) {
  cdf <- stats::ecdf(x)(x)
  cdf - adjust / (2 * length(x))
}


#' Read common file types
#'
#' @param path Path to files
#' @param full.names Return full file names?
#' @param recursive Recurse into sub-directories?
#' @export
list_tsvs <- function(path = '.', full.names = FALSE, recursive = FALSE)
  list.files(path = path,
    pattern = '\\.tsv$',
    full.names = full.names,
    recursive = recursive)

#' @rdname list_tsvs
#' @inheritParams list_tsvs
#' @export
list_csvs <- function(path = '.', full.names = FALSE, recursive = FALSE)
  list.files(path = path,
    pattern = '\\.csv$',
    full.names = full.names,
    recursive = recursive)

#' @rdname list_tsvs
#' @inheritParams list_tsvs
#' @export
list_txts <- function(path = '.', full.names = FALSE, recursive = FALSE)
  list.files(path = path,
    pattern = '\\.txt$',
    full.names = full.names,
    recursive = recursive)

#' @rdname list_tsvs
#' @inheritParams list_tsvs
#' @export
list_vbrs <- function(path = '.', full.names = FALSE, recursive = FALSE)
  list.files(path = path,
    pattern = '\\.vbr$',
    full.names = full.names,
    recursive = recursive)


#' Color helper for use with ggplot
#'
#' @param n Number of colors
#' @param deg Degrees away from 0 on the color wheel
#' @param l Luminosity
#' @export
color_hue <- function(n, deg = 15, l = 65) {
  hues = seq(deg, deg + 360, length = n + 1)
  hcl(h = hues, l = l, c = 100)[1:n]
}
