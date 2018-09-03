# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Prefer dplyr functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
filter <- dplyr::filter

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Globals
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
weekdays <- c("mo", "tu", "we", "th", "fr", "sa", "su")
ord_weekdays <- factor(weekdays, levels = weekdays)
next_weekdays <-  c("tu", "we", "th", "fr", "sa", "su", "mo")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Replace NA's by preceding label
# Example: c("label-A", NA, NA, "label-B", NA) is converted to
#          c("label-A", "label-A", "label-A", "label-B", "label-B") 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
repair_missing_names <- function(session_names) {
  result = NA
  i1 = 0
  sav_name = NA
  
  for (name in session_names) {
    i1 = i1 + 1
    
    if (!is.na(name)) {
      sav_name = name
      result[i1] = name
    } else {
      result[i1] = sav_name
    }
  }
  
  return(result)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Adjust session start: move broken hours forward to Top Of The Hour
#
# IN  time - chr(5), start of session
#                    format hh:mm (assume valid, by courtesy of Hijack)
#     day  - chr(2), a valid weekday name (mo, tu, ... su)
#
# OUT result - date, the adjusted session start with a dummy date
#                    
# Examples: adjust_start("19:58", "tu")
#           result = 
#           23:59 converts to 00, next day
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
adjust_start <- function(starttime, day) {
  start_hh = as.integer(str_sub(starttime, 1, 2))
  start_mm = as.integer(str_sub(starttime, 4, 5))
  start_day = day
  
  if (1 <= start_mm & start_mm <= 55){
    stop(paste0("minutes of starttime should be TOTH or slightly before: ", starttime))
  }
  
  if (start_mm != 0) {
    start_mm = 0
    start_hh = start_hh + 1 # modulo operator would hide day correction
    
    if (start_hh == 24) {
      start_hh = 0
      start_day <<- next_weekdays[which(weekdays %in% as.vector(day))]
    }
  }
  
  return(str_c(as.character(start_hh), ", ",
               as.character(start_mm), ", ",
               start_day, "."
  ))
}
# get the exports ----
sessions_export_uz <- stri_read_lines("data/hijack_export_uz.txt")
sessions_export_lg <- stri_read_lines("data/hijack_export_lg.txt")

# get the session names ----
pat_session <- "^\\(\\*session = (.*),.*\\)$"
extr_session_names_uz <- str_match(sessions_export_uz, pattern = pat_session)[, 2]
extr_session_names_lg <- str_match(sessions_export_lg, pattern = pat_session)[, 2]

# repair missing session names ----
session_names_uz <- repair_missing_names(extr_session_names_uz)
session_names_lg <- repair_missing_names(extr_session_names_lg)
rm(extr_session_names_uz, extr_session_names_lg)

# add session names ----
# as column to export data, preserving the originating machine
session_names_uz <- as.data.frame(session_names_uz)
sessions_export_uz <- as.data.frame(sessions_export_uz)
sessions_uz <- bind_cols(session_names_uz, sessions_export_uz) %>% 
  mutate(mac = "uz") %>% 
  select(session_names = session_names_uz,
         session_export = sessions_export_uz,
         mac)
rm(session_names_uz, sessions_export_uz)

session_names_lg <- as.data.frame(session_names_lg)
sessions_export_lg <- as.data.frame(sessions_export_lg)
sessions_lg <- bind_cols(session_names_lg, sessions_export_lg) %>% 
  mutate(mac = "lg") %>% 
  select(session_names = session_names_lg,
         session_export = sessions_export_lg,
         mac)
rm(session_names_lg, sessions_export_lg)

# unite lg/uz-sessions ----
sessions <- bind_rows(sessions_uz, sessions_lg)
rm(sessions_uz, sessions_lg)

# only keep timestamp rows ----
# so containing an ":"
sessions %<>% 
  dplyr::filter(str_detect(session_export, pattern = "^.*:.*$"))

# extract days ... ----
# and start and duration and add to sessions 
pat_start_dur <- "^.*= (.*) from (.{5}).*\\(for (\\d+)"
period <- str_match(sessions$session_export, pattern = pat_start_dur)
sessions %<>% 
  mutate(start = period[, 3],
         hours = period[, 4],
         days = period[, 2])

# normalize day names ----
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

# only keep normalized rows ... ----
# with normalized day names and not containing "disabled" or "expired" 
sessions %<>% 
  filter(str_detect(days, pattern = "mo|tu|we|th|fr|sa|su")) %>% 
  filter(str_detect(session_export, pattern = "^((?!Disabled|Expired).)*$")) 

# prep & gather days ...----
# (cols to rows), keeping only non-empty days 
days_by_col <- str_split(sessions$days, pattern = " ", simplify = TRUE, n = 7) %>% as.data.frame
sessions <-
  bind_cols(sessions, days_by_col) %>%
  gather(key = "day_column", value = "day", V1:V7) %>%
  filter(day != "")

# minimize to essential variables ----
# make hours numeric and order day names
sessions %<>% 
  select(session_names, mac, day_names = day, start, hours) %>% 
  mutate(hours = as.numeric(hours),
         day_names = factor(day_names, levels = ord_weekdays)) %>% 
  arrange(day_names, start, hours)

# add begin and end of logging ----
# = timestamps; rounds to nearest hour and corrects day if start = 23:xx (xx >= 30)
sessions %<>% mutate(begin = adjust_start(start, day_names))
