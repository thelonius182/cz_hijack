library(readr)
library(stringr)
library(rebus)
library(dplyr)


hijack_sessions_uz <- 
  read_delim(
    "data/hijack_export_uz.txt",
    "\t",
    escape_double = FALSE,
    col_names = c("script"),
    trim_ws = TRUE
  )

hijack_sessions_lg <-
  read_delim(
    "data/hijack_export_lg.txt",
    "\t",
    escape_double = FALSE,
    col_names = c("script"),
    trim_ws = TRUE
  )

session <- OPEN_PAREN %R% STAR %R% "session = " %R% capture(one_or_more(ANY_CHAR)) %R% 
  "," %R% one_or_more(ANY_CHAR) %R% CLOSE_PAREN %R% END
session <- OPEN_PAREN %R% STAR %R% "session = " 
str_match_all(hijack_sessions_uz$script, pattern = session)
sub("\\(\\*session = (.*),.*\\)", "\\1", hijack_sessions_uz, perl=TRUE, ignore.case=TRUE)
