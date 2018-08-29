library(stringi)
library(stringr)

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

sessions <- bind_rows(sessions_uz, sessions_lg)
# only keep lines not containing "button" or "session", using negative look-around
sessions <- filter(sessions, str_detect(session_export, "^((?!button|session).)*$"))

