library(baseballr)
library(readr)
library(dplyr)

file       <- "pitch_data_full.csv"
checkpoint <- "statcast_last_date.txt"

# Every date in the March 1 - Nov 1 window for each season (ends yesterday so
# games still in progress aren't saved as incomplete)
days <- seq(as.Date("2021-03-01"), Sys.Date() - 1, by = "day")
md   <- format(days, "%m-%d")
days <- days[md >= "03-01" & md <= "11-01"]

# Resume after the last completed date
if (file.exists(checkpoint) && file.exists(file)) {
  days <- days[days > as.Date(readLines(checkpoint, n = 1))]
}

pull_day <- function(d) {
  tryCatch(
    statcast_search(start_date = as.character(d), end_date = as.character(d)),
    error = function(e) { message("Failed: ", conditionMessage(e)); NULL }
  )
}

# Fixed header: use the existing file's header if there is one,
# otherwise the columns from the most recent day that has data
if (file.exists(file)) {
  header <- names(read_csv(file, n_max = 0, show_col_types = FALSE))
} else {
  header <- NULL
  for (k in 1:14) {
    ref <- pull_day(Sys.Date() - k)
    if (!is.null(ref) && nrow(ref) > 0) { header <- names(ref); break }
  }
  if (is.null(header)) stop("Couldn't get a reference header. Rerun later.")
}

for (i in seq_along(days)) {
  d <- days[i]                       # indexing keeps the Date class
  message("Downloading: ", d)
  
  dat <- pull_day(d)
  if (is.null(dat)) stop("Failed request on ", d, ". Rerun to resume.")
  
  if (nrow(dat) > 0) {
    # Add any missing columns as NA, drop extras, force the header order
    missing <- setdiff(header, names(dat))
    dat[missing] <- NA
    dat <- dat[, header]
    
    write_csv(dat, file, na = "",
              append = file.exists(file),
              col_names = !file.exists(file))
  }
  
  writeLines(as.character(d), checkpoint)
  Sys.sleep(1)
}

# Dedupe on the pitch key, using data.table to keep memory manageable 
if (requireNamespace("data.table", quietly = TRUE)) {
  dt <- data.table::fread(file)
  dt <- unique(dt, by = c("game_pk", "at_bat_number", "pitch_number"))
  data.table::fwrite(dt, file)
}

message("Statcast extraction complete.")