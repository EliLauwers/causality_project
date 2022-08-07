custom_pad = function(x) {
  rounded = trimws(as.character(format(round(x, 3), nsmall = 3)))
  padded = sapply(
    rounded,
    str_pad,
    width = nchar("-0.000"),
    pad = " ",
    simplify = T
  )
  glue::glue("[{paste0(padded,collapse=',')}]")
}