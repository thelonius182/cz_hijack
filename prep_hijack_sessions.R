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
# Build session period from session start etc.
# 
# Move broken hours forward to Top Of The Hour.
#
# NB - day enters the function as a number instead of a chr (so 1 for mo etc)
#      this is due to it being a factor; it needes to be a factor to allow
#      correct weekday ordering (not lexcial!)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
build_period <- function(day, starttime, duration) {
  start_hh = as.integer(str_sub(starttime, 1, 2))
  start_mm = as.integer(str_sub(starttime, 4, 5))
  duration = as.integer(duration)
  start_day = ord_weekdays[day] 
  if (1 <= start_mm & start_mm <= 55){
    stop(paste0("invalid starttime: ", starttime, " (required: minutes in [0, 54:59]"))
  }
  
  if (start_mm != 0) {
    start_mm <- 0
    start_hh <- start_hh + 1 # modulo operator would hide day correction
    
    if (start_hh == 24) {
      start_hh <- 0
      start_day <- next_weekdays[which(weekdays == start_day)]
    }
  }
  
  stop_hh <- start_hh + duration
  
  if (stop_hh > 24) {
    message(paste0("Warning: session across 2 days detected (input = ", day, ", ", starttime, ", ", duration, ")"))
  }
  
  start_hh <- paste0("00", start_hh) %>% str_sub(., -2)
  start_mm <- paste0("00", start_mm) %>% str_sub(., -2)
  stop_hh <- paste0("00", stop_hh) %>% str_sub(., -2)
  
  return(str_c(start_day, "|", start_hh, ":", start_mm, "|", stop_hh, ":00"))
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
  mutate(server = "uz") %>% 
  select(session_names = session_names_uz,
         session_export = sessions_export_uz,
         server)
rm(session_names_uz, sessions_export_uz)

session_names_lg <- as.data.frame(session_names_lg)
sessions_export_lg <- as.data.frame(sessions_export_lg)
sessions_lg <- bind_cols(session_names_lg, sessions_export_lg) %>% 
  mutate(server = "lg") %>% 
  select(session_names = session_names_lg,
         session_export = sessions_export_lg,
         server)
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
  filter(day != "") %>% 
  select(session_names, server, day, start, hours) %>% 
  mutate(day = factor(day, levels = ord_weekdays),
         server = factor(server, levels = c("uz", "lg"), labels = c("uitzend-mac", "log-mac"))
  ) %>% 
  arrange(day, start) 

# add begin and end of sessions ----
# = timestamps; rounds to nearest hour and corrects day if start = 23:xx (xx >= 30)
periods <- pmap( list(sessions$day, sessions$start, sessions$hours), build_period) %>% unlist

sessions %<>% mutate(period = periods) %>% 
  separate(col = period, into = c("period_dag", "period_begin", "period_end"), sep = "\\|", remove = T) %>% 
  separate(col = period_begin, into = c("period_begin_hh", "period_begin_mm"), sep = ":", remove = T) %>% 
  separate(col = period_end, into = c("period_end_hh", "period_end_mm"), sep = ":", remove = T) %>% 
  rename(lengte = hours, sessienaam = session_names, dag = day) %>% 
  select(sessienaam, server, dag, period_begin_hh, period_end_hh, lengte) %>% 
  mutate(begin = as.integer(period_begin_hh), eind = as.integer(period_end_hh)) %>% 
  select(sessienaam, server, dag, begin, eind, lengte) %>% 
  mutate(dag = factor(dag,
                      levels = rev(c("su", "sa", "fr", "th", "we", "tu", "mo")),
                      labels = rev(c("zondag",
                                 "zaterdag",
                                 "vrijdag",
                                 "donderdag",
                                 "woensdag",
                                 "dinsdag",
                                 "maandag"))
         ),
         sessienaam = factor(sessienaam)
  )
