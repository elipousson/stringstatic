str_split <- function(string, pattern, n = Inf, simplify = FALSE) {
  if (simplify) {
    # return(matrix(str_split_fixed(string, pattern, 5), ncol = 5))
  }

  is_fixed <- inherits(pattern, "fixed")

  string <-
    strsplit(
      x = string,
      split = pattern,
      fixed = is_fixed,
      perl = !is_fixed,
    )

  if (!identical(n, Inf)) {
    string <- lapply(
      string,
      function(x) {
        x[1:min(length(x), n)]
      }
    )
  }

  string
}

str_split_fixed <- function(string, pattern, n) {
  stopifnot(
    !missing(n)
  )

  string <- str_split(string, pattern, n)

  lapply(
    string,
    function(x) {
      if (length(x) < n) {
        c(x, rep("", n - length(x)))
      } else {
        x
      }
    }
  )
}
