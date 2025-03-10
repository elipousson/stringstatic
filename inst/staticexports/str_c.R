#' Join multiple strings into a single string
#'
#' Dependency-free drop-in alternative for `stringr::str_c()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param ... One or more character vectors.
#'   Zero length arguments are removed.
#'   Short arguments are recycled to the length of the longest.
#'
#'   Like most other R functions, missing values are "infectious":
#'   whenever a missing value is combined with another string
#'   the result will always be missing.
#'   Use `str_replace_na()` to convert `NA` to "NA"
#'
#' @param sep String to insert between input vectors.
#'
#' @param collapse
#'   Optional string used to combine input vectors into single string.
#'
#' @return If `collapse = NULL` (the default) a character vector
#'   with length equal to the longest input string.
#'   If collapse is non-`NULL`, a character vector of length 1.
#' @noRd
str_c <- function(..., sep = "", collapse = NULL) {
	stopifnot(
		"`sep` must be a single string, not a character vector." = length(sep) == 1,
		"`collapse` must be a single string or `NULL`, not a character vector." =
			length(collapse) == 1 || is.null(collapse)
	)

	strings <- Filter(function(x) !is.null(x), list(...))

	if (length(strings) == 0 || any(lengths(strings) == 0)) {
		if (length(collapse) == 0) return(character(0))
		return("")
	}

	max_length <- max(lengths(strings))

	result <- lapply(strings, rep_len, length.out = max_length)
	result <- do.call(cbind, result)
	result <- apply(result, 1, paste, collapse = sep)
	result <- paste(result, collapse = collapse)

	result
}
