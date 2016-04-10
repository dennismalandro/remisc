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


#' Weibull transform
#'
#' @export
weibull_transform <- function(p) log(-log(1 - p))
#' @export
weibull_inverse_transform <- function(x) 1 - exp(-exp(x))
#' @export
weibull_trans <- function()
  trans_new("weibull",
            weibull_transform,
            weibull_inverse_transform)

#' Arrhenius transform
#'
#' @export
arrhenius_transform <- function(degC) 1 / (degC + 273.15)
#' @export
arrhenius_inverse_transform <- function(Arr) 1 / Arr - 273.15
#' @export
arrhenius_trans <- function()
  trans_new("arrhenius",
            arrhenius_transform,
            arrhenius_inverse_transform)


#' User-friendly X-vector generator for DOE
#' Ironically, not factors
#' @export
make_x <- function(levels = c('lo', 'hi'), reps = 1, n = length(levels) * reps) {
  if(length(n) > 1) n <- length(n)
  gl(n = length(levels), k = reps, length = n, labels = levels) %>%
    as.character
}
