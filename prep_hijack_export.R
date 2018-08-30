library(stringi)
library(stringr)
library(lubridate)

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

round_to_nearest_hour <- function(time_string, type) {
  # expects a string like "14:01"
  a_date_string <- paste0("1958-12-25 ", time_string, ":00.00")
  a_date <- ymd_hms(a_date_string)
  to_nearest_hour <- round_date(a_date, "hour")
  result <- as.integer(hour(to_nearest_hour))
  
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
# only keep lines having some timestamp, so containing an ":"
sessions <- filter(sessions, str_detect(session_export, "^.*:.*$"))

pat_period <- "^.*= (.*) from (.{5}) to (.{5})"
period <- str_match(sessions$session_export, pattern = pat_period)

round_to_nearest_hour("23:58", "begin")
