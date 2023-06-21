# checks which files have more than one sync time

#  Load readxl package
library(readxl)

# Set path to directory containing Excel files
path <- "iPad"

# Get list of Excel files in directory
excel_files <- list.files(path = path, pattern = "*.xlsx", full.names = TRUE)

# Loop through Excel files and check number of rows
for (file in excel_files) {
  # Read Excel file using read_excel function from readxl package
  sheet <- read_excel(file, sheet = 1)
  
  # Check number of rows in sheet
  num_rows <- nrow(sheet)
  
  # Check if there are only 2 rows in sheet, print file name if not
  if (num_rows != 2) {
    print(paste("File:", file, "has", num_rows, "rows"))
  }
}

# Checks which files have sync times that are not on the same day and not within 5 minutes of each other

# Set path to directory containing Excel files
path <- "iPad"

# Get list of Excel files in directory
excel_files <- list.files(path = path, pattern = "*.xlsx", full.names = TRUE)

# Loop through Excel files and check number of rows
for (file in excel_files) {
  # Read Excel file using read_excel function from readxl package
  sheet <- read_excel(file, sheet = 1)
  
  data = sheet|>
  group_by(device) |> 
  mutate(sync = row_number(), time = mdy_hms(time)) |> 
  group_by(sync) |> 
  pivot_wider(names_from = "device", values_from = "time") |> 
  ungroup() |> 
  mutate(diff = as.numeric(difftime(ipad, Capnostream, units = "mins")), same_day = as.Date(ipad) == as.Date(Capnostream)) 
  
  same_day = data|>
  pull(same_day)

  diffTime = data|>
  pull(diff)
  
  # Check if there are only 2 rows in sheet, print file name if not
  if (any(!same_day)) {
    print(paste("File:", file, "has sync times that are not on the same day"))
  }
  if (any(diffTime > 5)) {
    print(paste("File:", file, "has sync times that are not within 5 minutes", diffTime))
  }
}

# Checks which files have more than one nurse

#  Load readxl package
library(readxl)

# Set path to directory containing Excel files
path <- "iPad"

# Get list of Excel files in directory
excel_files <- list.files(path = path, pattern = "*.xlsx", full.names = TRUE)

# Loop through Excel files and check number of rows
for (file in excel_files) {
  # Read Excel file using read_excel function from readxl package
  sheet <- read_excel(file, sheet = "Nurse ID")
  
  # Check number of rows in sheet
  num_rows <- nrow(sheet)
  
  # Check if there are only 2 rows in sheet, print file name if not
  if (num_rows != 1) {
    print(paste("File:", file, "has", num_rows, "rows"))
  }
}


# Checks which files have more than one intervention

# Set path to directory containing Excel files
path <- "iPad"

# Get list of Excel files in directory
excel_files <- list.files(path = path, pattern = "*.xlsx", full.names = TRUE)

# Loop through Excel files and check number of rows
for (file in excel_files) {
  # Read Excel file using read_excel function from readxl package
  sheet <- read_excel(file, sheet = "Interventions")
  
  # Check number of rows in sheet
  num_rows <- nrow(sheet)
  
  # Check if there are only 2 rows in sheet, print file name if not
  if (num_rows != 0) {
    print(paste("File:", file, "has", num_rows, "rows"))
  }
}
# Checks which files have more than one troops event

# Set path to directory containing Excel files
path <- "iPad"

# Get list of Excel files in directory
excel_files <- list.files(path = path, pattern = "*.xlsx", full.names = TRUE)

# Loop through Excel files and check number of rows
for (file in excel_files) {
  # Read Excel file using read_excel function from readxl package
  sheet <- read_excel(file, sheet = "Adverse events")
  
  # Check number of rows in sheet
  num_rows <- nrow(sheet)
  
  # Check if there are only 2 rows in sheet, print file name if not
  if (num_rows != 1) {
    print(paste("File:", file, "has", num_rows, "rows"))
  }
}
