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

df.xlsx.flat <- unlist(df.xlsx, recursive = FALSE) #flatten list
df.xlsx.sel <- df.xlsx.flat[c(1,2,4,6,9,11,13,14,15,16,18,20,22)] #select dfs of interest

#### Dataframes ####

# Main ####
## After comparison keep Sp_name from v02, and tidy
df.maintraitsv02 <- df.maintraitsv02 %>% 
  slice(-(1:3)) 
colnames(df.maintraitsv02) <- c("notes", "Sp_name")


# Bat_Data_Sites ####
## After comparison, don't need `_Summary`
df.dsites <- df.xlsx.sel[["Bat_Data_Sites_20230802.xlsx.Data"]]

df.dsites.na <- df.dsites              #new df for NA
df.dsites.na[df.dsites.na == 0] <- NA  #make 0s NA

df.dsites.long <- df.dsites.na %>% 
  pivot_longer(cols = 9:252, names_to = "SpeciesID", values_to="Pres", values_drop_na=TRUE) #drop NA- no rows for them

df.dsites.cut <- df.dsites.long %>% 
  group_by(Study, continent, Site_name, SpeciesID) %>% slice_head()  #keep only 1 row per each grp var

df.dsites.continent <- df.dsites.long %>% 
  group_by(continent, SpeciesID) %>% slice_head() %>% ungroup()  #keep only species per continent



# Matrix ####
## Get SiteID from `Bat_sites` and append to `Bat_species`
df.matrix <- df.xlsx.sel[["Bat_matrices.xlsx.Bat_sites"]] %>% 
  select(1:2) %>% 
  left_join(df.xlsx.sel[["Bat_matrices.xlsx.Bat_species"]], .) %>% 
  relocate(City, .after=SiteID)  


# Initial data ####
df.incurated <- df.csv[["data/speciesID/Initial_data_bats_curated.csv"]]

df.incurated.na <- df.incurated
df.incurated.na[df.incurated.na == 0] <- NA

df.incurated.long <- df.incurated.na %>% 
  pivot_longer(cols = 7:89, names_to = "SpeciesID", values_to="Pres", values_drop_na=TRUE) %>% 
  select(1:6, SpeciesID, Pres)

## Site names with issues due to special characters
df.incurated.fix <- df.incurated.long %>% 
  mutate(Site_name = case_when(str_detect(Site_name, "ich") ~ "Zurich",
                               str_detect(Site_name, "Bras do Regedouro") ~ "Sao Bras do Regedouro_S. Bras",
                               str_detect(Site_name, "Brissos_S. Brissos") ~ "Sao Brissos_S. Brissos",
                               .default=Site_name))

## Break up sites only on final delimiter
df.incurated.sep <- df.incurated.fix %>% 
  separate(Site_name, into = c("Site1", "Site2"), sep = "\\_(?!.*_)", remove = FALSE)



