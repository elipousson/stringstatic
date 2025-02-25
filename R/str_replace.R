# `R/str_replace.R` is imported from `inst/staticexports/str_replace.R`. 
# Please edit that file instead.

# TODO: Add support for function `replacement`

#' Replace matched patterns in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_replace()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @param replacement A character vector of replacements.
#'   Should be either length one, or the same length as `string` or `pattern`.
#'   References of the form `\1`, `\2`, etc. will be replaced with the contents
#'   of the respective matched group (created by `()`).
#'
#'   To replace the complete string with `NA`,
#'   use `replacement = NA_character_`.
#'
#'   Using a function for `replacement` is not yet supported.
#'
#' @return A character vector.
#' @export
str_replace <- function(string, pattern, replacement) {
	ignore.case <- isTRUE(attr(pattern, "options")$case_insensitive)
	is_fixed <- !ignore.case && inherits(pattern, "fixed")

	sub <- Vectorize(sub, c("pattern", "replacement", "x"), USE.NAMES = FALSE)

	sub(
		pattern,
		replacement,
		x = string,
		ignore.case = ignore.case,
		perl = !is_fixed,
		fixed = is_fixed
	)
}

#' Replace matched patterns in a string
#'
#' Dependency-free drop-in alternative for `stringr::str_replace_all()`.
#'
#' @source Adapted from the [stringr](https://stringr.tidyverse.org/) package.
#'
#' @param string Input vector.
#'   Either a character vector, or something coercible to one.
#'
#' @param pattern Pattern to look for.
#'
#'   The default interpretation is a regular expression,
#'   as described in [base::regex].
#'   Control options with [regex()].
#'
#'   Match a fixed string (i.e. by comparing only bytes), using [fixed()].
#'   This is fast, but approximate.
#'
#' @param replacement A character vector of replacements.
#'   Should be either length one, or the same length as `string` or `pattern`.
#'   References of the form `\1`, `\2`, etc. will be replaced with the contents
#'   of the respective matched group (created by `()`).
#'
#'   To perform multiple replacements in each element of `string`,
#'   pass a named vector `(c(pattern1 = replacement1))` to `str_replace_all()`.
#'
#'   To replace the complete string with `NA`,
#'   use `replacement = NA_character_`.
#'
#'   Using a function for `replacement` is not yet supported.
#'
#' @return A character vector.
#' @export
str_replace_all <- function(string, pattern, replacement) {
	ignore.case <- isTRUE(attr(pattern, "options")$case_insensitive)
	is_fixed <- !ignore.case && inherits(pattern, "fixed")

	if (!is.null(names(pattern))) {
		for (i in seq_along(pattern)) {
			string <- gsub(
				pattern = names(pattern)[[i]],
				replacement = pattern[[i]],
				x = string,
				ignore.case = ignore.case,
				perl = !is_fixed,
				fixed = is_fixed
			)
		}

		return(string)
	}

	gsub <- Vectorize(gsub, c("pattern", "replacement", "x"), USE.NAMES = FALSE)

	gsub(
		pattern,
		replacement,
		x = string,
		ignore.case = ignore.case,
		perl = !is_fixed,
		fixed = is_fixed
	)
}

#' Turn NA into "NA"
#'
#' Dependency-free drop-in alternative for `stringr::str_replace_na()`.
#'
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param replacement A single string.
#'
#' @return A character vector.
#' @export
str_replace_na <- function(string, replacement = "NA") {
	string[is.na(string)] <- replacement
	string
}
