library(tidyverse)
library(arrow)
options(warn=1)
clean_files <- function(file) {
    cat("Processing file:", file, "\n")

  colNames <- c(
    "date",
    "time",
    "co2Wave",
    "etco2",
    "respRate",
    "ipi",
    "spo2",
    "pulseRate",
    "apneasPerHour",
    "oxygenDesatIndex",
    "etco2High",
    "etco2Low",
    "respRateHigh",
    "respRateLow",
    "noBreath",
    "ipiLow",
    "spo2High",
    "spo2Low",
    "pulseRateHigh",
    "pulseRateLow",
    "apnea10",
    "apnea19",
    "apnea29",
    "apnea30",
    "oxygenDesatProlonged",
    "co2NA",
    "spo2NA",
    "PrBatteryLow",
    "SecBatteryLow",
    "events"
  )

  randomization = tribble(
  ~ID, ~randomization,
  "N01", "IPI enabled",
  "N02", "IPI disabled",
  "N03", "IPI disabled",
  "N04", "IPI enabled",
  "N05", "IPI disabled",
  "N06", "IPI disabled",
  "N07", "IPI enabled",
  "N08", "IPI enabled",
  "N09", "IPI disabled",
  "N10", "IPI enabled",
  "N11", "IPI disabled",
)

  diff_time <- readxl::read_excel(paste0("iPad/", str_remove(file, ".csv"), ".xlsx"), sheet = "Sync times") |>
    dplyr::group_by(device) |>
    mutate(sync = row_number(), time = mdy_hms(time)) |>
    group_by(sync) |>
    pivot_wider(names_from = "device", values_from = "time") |>
    ungroup() |>
    mutate(
      diff = difftime(
        ipad,
        Capnostream,
        units = "secs"
      )
    ) |>
    # to add the diff to the monitor data based on the sync time
    mutate(datetime = Capnostream) |>
    select(datetime, diff) |>
    arrange(datetime)

  nurse_times <- readxl::read_excel(paste0("iPad/", str_remove(file, ".csv"), ".xlsx"), sheet = "Nurse ID") |>
    mutate(time_corrected = mdy_hms(time)) |>
    arrange(time_corrected) |>
    select(-time)

  event_times <- readxl::read_excel(paste0("iPad/", str_remove(file, ".csv"), ".xlsx"), sheet = "Interventions") |>
    mutate(time_corrected = mdy_hms(time)) |>
    arrange(time_corrected) |>
    select(-time)

    # datetime is the time for analysis. the time_corrected column is the ipad time. that was needed so that the interventions and nurse column could be added at the correct locations

  monitor_data <- vroom::vroom(paste0("csv_data/", file),
    id = "path",
    delim = ",",
    skip = 6,
    # remove low battery and events columns
    col_names = colNames,
    col_types = c(
      date = "c",
      time = "c",
      ipi = "d",
      spo2 = "d",
      pulseRate = "d",
      oxygenDesatIndex = "d",
      events = "c"
    ),
    na = c("--", "")
  ) |>
    mutate(id = str_remove(path, ".csv")) |>
    mutate(id = str_remove(id, "csv_data/")) |>
    select(id, everything(), -path) |>
    mutate(datetime = mdy_hms(paste(date, time))) |>
    # the first sync time may be before the time in the first row of monitor_data (need to check?)
    add_row(datetime = diff_time$datetime[1], .before = 1) |>
    full_join(diff_time, by = "datetime") |>
    # fill in the diff times first down but then up in case the first sync time is not right at the first row
    fill(diff, .direction = "downup") |>
    # once filled we don't need the added first row anymore
    filter(!is.na(date)) |>
    # make a new column with the time corrected for the difference between the monitor and the ipad
    mutate(time_corrected = datetime + diff) |>
    select(-diff) |>
    # add the nurse times
    add_row(datetime = nurse_times$time_corrected[1], .before = 1) |>
    full_join(nurse_times, by = "time_corrected") |>
    fill(nurse, .direction = "downup") |>
    # once filled we don't need the added first row anymore
    filter(!is.na(date)) |>
    # add the event times
    left_join(event_times, by = "time_corrected") |>
    # only need 1 row per second
    distinct(datetime, .keep_all = T)  |>
    #add a column called randomization that looks up the nurse_id in the randomization table
    left_join(randomization, by = c("nurse" = "ID"))



  pt <- monitor_data$id[1]


  if (!dir.exists("monitor_data")) {
    dir.create("monitor_data")
  }
  if (!dir.exists(paste0("monitor_data/", pt))) {
    dir.create(paste0("monitor_data/", pt))
  }
  write_parquet(monitor_data, sink = paste0("monitor_data/", pt, "/part-0.parquet"))
}

files <- list.files("csv_data")

walk(files, clean_files)
