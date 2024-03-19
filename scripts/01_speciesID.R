pacman::p_load(
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  matchmaker, # dictionary-based cleaning
  tidyverse,  # data management and visualization
  readxl,
  openxlsx,
  tools,
  purrr,
  fuzzyjoin,
  stringdist,
  gt
)


#### Read sheets ####

## Load CSVs ####
sp.sheets.csv <- fs::dir_ls("data/speciesID", glob="*.csv")

df.csv <- sp.sheets.csv %>% 
  map(read_csv)

## Load XLSXs ####

sp.sheets.xlsx <- file.path(fs::dir_ls("data/speciesID", glob="*.xlsx"))

# Function to read sheets
excel_sheet_reader <- function(filename) {
  sheets <- excel_sheets(filename)
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

# Read all docs using above function
df.xlsx <- sp.sheets.xlsx %>% 
  map(function (path) {
    excel_sheet_reader(path)})

# Set names
## `basename` gets just name of df without path
df.xlsx <- set_names(df.xlsx, basename(sp.sheets.xlsx))

