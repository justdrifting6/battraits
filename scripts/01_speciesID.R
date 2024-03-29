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

#### DATAFRAMES ####

# Main ####
## After comparison keep Sp_name from v02, and tidy
df.maintraitsv02o <- df.xlsx.sel[["20220619_Bat_traits_KJ.xlsx.Traits (averages)_v02"]][c(1,3)]

df.maintraitsv02 <- df.maintraitsv02o %>% 
  slice(-(1:2)) %>% slice(-(188:190)) 
colnames(df.maintraitsv02) <- c("notes", "Sp_name")


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

df.incurated.sep %>% count(Site1) %>%  gt()
df.incurated.sep %>% count(Site2) %>%  gt()


# TO USE: Bat_Data_Sites ####
## After comparison, don't need `_Summary`
df.dsites <- df.xlsx.sel[["Bat_Data_Sites_20230802.xlsx.Data"]]

df.dsites.na <- df.dsites              #new df for NA
df.dsites.na[df.dsites.na == 0] <- NA  #make 0s NA

#df.dsites.clean <- df.dsites.na %>% clean_names(case="sentence", sep=" ") ##issue here is it gives duplicate columns _2

df.dsites.long <- df.dsites.na %>% 
  pivot_longer(cols = 9:252, names_to = "SpeciesID", values_to="Pres", values_drop_na=TRUE) #drop NA- no rows for them

df.dsites.clean <- df.dsites.long %>% 
  mutate(SpeciesIDold = SpeciesID,
         SpeciesID = str_replace(SpeciesID, pattern="\\.(?!$)", replacement = " "),
         SpeciesID = str_replace(SpeciesID, pattern="\\.(?!$)", replacement = "\\-"),
         SpeciesID = str_replace(SpeciesID, pattern="\\.(?!$)", replacement = " "),
         SpeciesID = str_replace(SpeciesID, pattern=" (?=[A-Z])", replacement = "-"))  #standardise sp names

df.dsites.cut <- df.dsites.clean %>% 
  group_by(Study, continent, Site_name, SpeciesID) %>% slice_head()  #keep only 1 row per each grp var

df.dsites.continent <- df.dsites.clean %>% 
  distinct(continent, SpeciesID, Study, .keep_all=TRUE) %>% 
  group_by(continent, SpeciesID) %>% 
  mutate(Studies = toString(Study)) %>%  #bring all studies into a string
  slice_head() %>% ungroup() %>%  #keep only species per continent
  group_by(SpeciesID) %>% 
  mutate(continents = toString(continent)) %>%  #when multiple continents for Sp, make it a string
  ungroup() %>% 
  select(-c(Study, Study_group_id, Study_period_year, UniqueID, Site_name))

df.dsites.continent %>% filter(SpeciesID != SpeciesIDold) %>% arrange(SpeciesIDold) %>% view()
  
df.dsites.continent %>% filter(str_detect(SpeciesID, "-"))
df.maintraitsv02 %>% filter(str_detect(Sp_name, "Myotis emarginatus"))

#### SPECIES NAMES ####

sp.df.maintraits <- df.maintraitsv02 %>% 
  distinct(notes, Sp_name) %>% 
  mutate(Sp_name_join = str_replace_all(Sp_name, pattern="\\/", replacement = "-")) 
  
sp.df.dsites <- df.dsites.continent %>% 
  distinct(SpeciesID, .keep_all = TRUE) %>% 
  select(-SpeciesIDold)

sp.colnames <- df.xlsx.sel[["species_col_names.xlsx.Spp Names"]] %>% clean_names()

anti_join(sp.df.dsites, sp.df.maintraitsv02, join_by("SpeciesID" == "Sp_name"))
anti_join(sp.df.maintraitsv02, sp.df.dsites, join_by("Sp_name" == "SpeciesID"))
anti_join(sp.df.dsites, sp.colnames, join_by("SpeciesID" == "desired_name"))
anti_join(sp.df.maintraitsv02, sp.colnames, join_by("Sp_name" == "desired_name"))

sp.join <- full_join(sp.df.maintraits, sp.df.dsites, join_by("Sp_name_join" == "SpeciesID"), keep=TRUE) %>% 
  relocate(SpeciesID, .after=Sp_name_join)

sp.join.mis <- sp.join %>% 
  filter(is.na(Sp_name) | is.na(SpeciesID)) %>% 
  mutate(comb = coalesce(Sp_name, SpeciesID))  #sp that aren't joining. 
  
write_csv(sp.join, "process/20240325_species_locat.csv")

