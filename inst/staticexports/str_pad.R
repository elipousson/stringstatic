str_pad <- function(string, width, side = c("left", "right", "both"), pad = " ") {
  stopifnot(
  	is.numeric(width),
    width > 0,
    nchar(pad) == 1
  )

  side <- match.arg(side)

  rep_pad <- function(width) {
  	vapply(
  		width,
  		function(x) {
  			paste0(rep.int(pad, max(x, 0)), collapse = "")
  		},
  		""
  	)
  }

  pad_side <- width - nchar(string)
  pad_front <- pad_side %/% 2

  switch(side,
    "left" = paste0(rep_pad(pad_side), string),
    "right" = paste0(string, rep_pad(pad_side)),
    "both" = paste0(rep_pad(pad_front), string, rep_pad(pad_side - pad_front))
  )
}
