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


# Read sheets

sp.sheets.xlsx <- fs::dir_ls("data/speciesID", glob="*.xlsx")
sp.sheets.xlsx <- file.path(sp.sheets.xlsx)
sp.sheets.df <- as.data.frame(sp.sheets.xlsx)

sp.sheets.csv <- fs::dir_ls("data/speciesID", glob="*.csv")

## Load CSVs
sp.sheets.csv %>% 
  map(function (path) {
    read_csv(path)
  })

df.csv <- sp.sheets.csv %>% 
  map(read_csv)

### Load all excel sheets ----

df.xlsx <- sp.sheets.xlsx %>% map(read_excel)


path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)
#> $iris

sp.sheets.xlsx.s <- sp.sheets.xlsx %>% 
  map(function(sheet){
    readxl::excel_sheets(sheet)})

df.list <- lapply(sp.sheets.xlsx, read_excel)

data_xlsx_df <- map_df(set_names(sp.sheets.xlsx), function(file) {
  file %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map(
      ~ read_xlsx(path = file, sheet = .x))
})

df.xlsx <- sp.sheets.xlsx %>%
  map(
    .f = function(path) {
      read_excel(
        path,          #making all cols text/character was only way I could get next section to work; will need to convert later
        .name_repair = "minimal"           #we've already fixed header names
      )
    })

excel_sheet_reader <- function(filename) {
  sheets <- excel_sheets(filename)
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

map(
  .f = function(path) {
    read_excel(
      path,
      sheet = "PGP MeasureSheet",
      range=anchored("A5", c(400, 23)),  #change no cols to match category settings- start at A5, bring in 23 cols and 400 rows- this drops the `x`s in the last row
      col_names = header1,               #change header reference to match cat
      col_types = c("text"),             #making all cols text/character was only way I could get next section to work; will need to convert later
      .name_repair = "minimal"           #we've already fixed header names
    )
  })

plot.cat1.map <- plot.cat1.map %>% set_names(plot.cat1) #name dataframes by filename