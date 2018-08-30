library(stringi)
library(stringr)
library(lubridate)
library(dplyr)
library(magrittr)
library(tidyr)

# -------------------------------------------------------------------------
# Replace NA's by preceding label
# Example: c("label-A", NA, NA, "label-B", NA) is converted to
#          c("label-A", "label-A", "label-A", "label-B", "label-B") 
# -------------------------------------------------------------------------
repair_missing_names <- function(session_names) {
  result = NA
  i1 = 0
  sav_session_name = NA
  
  for (session_name in session_names) {
    i1 = i1 + 1
    
    if (!is.na(session_name)) {
      sav_session_name = session_name
      result[i1] = session_name
    } else {
      result[i1] = sav_session_name
    }
    
  }
  
  return(result)
}

# -------------------------------------------------------------------------
# Move clock to the nearest hour. 
# parm Type indicates whether this hour starts or stops a period.
#           Needed for rounding to "midnight" - start of event: round to 0
#                                             - end: round to 24
# Examples: "19:01" converts to 19
#           "19:56" converts to 20
#           "00:01" and "23:58" convert to 0 if type=begin
#           "00:01" converts to 24 if type=end
# -------------------------------------------------------------------------
round_to_nearest_hour <- function(time_string, type) {
  # expects a string like "14:01"
  a_date_string <- paste0("1958-12-25 ", time_string, ":00.00")
  a_date <- ymd_hms(a_date_string)
  nearest_hour <- round_date(a_date, "hour")
  result <- as.integer(hour(nearest_hour))
  
  if (result == 0 & type == "end") {
    result = 24
  }
  
  return(result)
}

sessions_export_uz <- stri_read_lines("data/hijack_export_uz.txt")
sessions_export_lg <- stri_read_lines("data/hijack_export_lg.txt")

pat_session <- "^\\(\\*session = (.*),.*\\)$"

extr_session_names_uz <- str_match(sessions_export_uz, pattern = pat_session)[, 2]
session_names_uz <- repair_missing_names(extr_session_names_uz)
rm(extr_session_names_uz)
session_names_uz <- as.data.frame(session_names_uz)
sessions_export_uz <- as.data.frame(sessions_export_uz)
sessions_uz <- bind_cols(session_names_uz, sessions_export_uz) %>% 
  mutate(mac = "uz") %>% 
  select(session_names = session_names_uz,
         session_export = sessions_export_uz,
         mac)
rm(session_names_uz, sessions_export_uz)

extr_session_names_lg <- str_match(sessions_export_lg, pattern = pat_session)[, 2]
session_names_lg <- repair_missing_names(extr_session_names_lg)
rm(extr_session_names_lg)
session_names_lg <- as.data.frame(session_names_lg)
sessions_export_lg <- as.data.frame(sessions_export_lg)
sessions_lg <- bind_cols(session_names_lg, sessions_export_lg) %>% 
  mutate(mac = "lg") %>% 
  select(session_names = session_names_lg,
         session_export = sessions_export_lg,
         mac)
rm(session_names_lg, sessions_export_lg)

sessions <- bind_rows(sessions_uz, sessions_lg)
rm(sessions_uz, sessions_lg)
# only keep rows having some timestamp, so containing an ":"
sessions <- filter(sessions, str_detect(session_export, "^.*:.*$"))

# extract days, start and duration and add to sessions
pat_start_len <- "^.*= (.*) from (.{5}).*\\(for (\\d+)"
period <- str_match(sessions$session_export, pattern = pat_start_len)
sessions %<>% 
  mutate(start = period[, 3],
         hours = period[, 4],
         days = period[, 2])

# normalize day names
sessions$days %<>% 
  str_to_lower %>% 
  str_replace_all(pattern = "weekdays", "mo, tu, we, th, fr") %>% 
  str_replace_all(pattern = "mondays", "mo") %>% 
  str_replace_all(pattern = "tuesdays", "tu") %>% 
  str_replace_all(pattern = "wednesdays", "we") %>% 
  str_replace_all(pattern = "thursdays", "th") %>% 
  str_replace_all(pattern = "fridays", "fr") %>% 
  str_replace_all(pattern = "saturdays", "sa") %>% 
  str_replace_all(pattern = "sundays", "su") %>% 
  str_replace_all(pattern = ",", "") %>% 
  str_replace_all(pattern = " and ", " ") 

# only keep rows with normalized day names (effectively this removes all once-only sessions)
sessions %<>% 
  filter(str_detect(days, pattern = "mo|tu|we|th|fr|sa|su"))

# prep & gather days (cols to rows), keeping only non-empty days
days <- str_split(sessions$days, pattern = " ", simplify = TRUE, n = 7) %>% as.data.frame
sessions <-
  bind_cols(sessions, days) %>%
  gather(key = "session_day", value = "day", V1:V7) %>%
  filter(day != "")

sessions %<>% 
  select(session_names, mac, start, hours, day_names = day) %>% 
  mutate(hours = as.numeric(hours))


# round_to_nearest_hour("23:58", "begin")
