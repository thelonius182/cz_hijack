library(readr)
hijack_sessions <-
  read_delim(
    "data/hijack_export.txt",
    "\t",
    escape_double = FALSE,
    col_names = c("script"),
    trim_ws = TRUE
  )
