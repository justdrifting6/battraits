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

# Main
df.maintraitsv01 <- df.xlsx.sel[["20220619_Bat_traits_KJ.xlsx.Traits (averages)_v01"]][1:3]
df.maintraitsv02 <- df.xlsx.sel[["20220619_Bat_traits_KJ.xlsx.Traits (averages)_v02"]][c(1,3)]

## Compare
anti_join(df.maintraitsv01, df.maintraitsv02) 
anti_join(df.maintraitsv02, df.maintraitsv01)
anti_join(df.maintraitsv02, df.maintraitsv01, join_by(`...3` == `...2`))

## Keep Sp_name from v02, and tidy
df.maintraitsv02 <- df.maintraitsv02 %>% 
  slice(-(1:3)) 
colnames(df.maintraitsv02) <- c("notes", "Sp_name")

# 20230802
df.data.sites <- df.xlsx.sel[["Bat_Data_Sites_20230802.xlsx.Data"]][1]
df.data.summary <- df.xlsx.sel[["Bat_Data_Summary_20230802.xlsx.Bat_Data_20230802"]][1]
anti_join(df.data.sites, df.data.summary)

s.sites <- df.data.sites %>% group_by(Study) %>% count() %>% ungroup()
s.sum <- df.data.summary %>% group_by(Study) %>% count() %>% ungroup()
tt <- compare_df_cols(df.xlsx.sel[["Bat_Data_Summary_20230802.xlsx.Bat_Data_20230802"]], df.xlsx.sel[["Bat_Data_Sites_20230802.xlsx.Data"]])

## Keep just Sites
df.data.sites <- df.xlsx.sel[["Bat_Data_Sites_20230802.xlsx.Data"]]

#df.datasi.clean <- df.data.sites %>% janitor::clean_names() <- makes dupe columns _2 so not ideal

df.datasi.na <- df.data.sites
df.datasi.na[df.datasi.na == 0] <- NA

df.datasi.long <- df.datasi.na %>% 
  pivot_longer(cols = 9:252, names_to = "SpeciesID", values_to="Pres", values_drop_na=TRUE) 

df.datasi.cut <- df.datasi.long %>% 
  group_by(Study, continent, Site_name, SpeciesID) %>% slice_head()  #keep only 1 row per each grp var

df.datasi.cont <- df.datasi.long %>% 
  group_by(continent, SpeciesID) %>% slice_head() %>% ungroup()

df.datasi.cont %>% count(continent)
df.datasi.cont %>% filter(continent == "Europe") %>% count(SpeciesID)
df.datasi.cont %>% filter(SpeciesID == "Barbastella.barbastellus")

# Matrix
df.matrix <- df.xlsx.sel[["Bat_matrices.xlsx.Bat_sites"]] %>% 
  select(1:2) %>% 
  left_join(df.xlsx.sel[["Bat_matrices.xlsx.Bat_species"]], .) %>% 
  relocate(City, .after=SiteID)

# Initial data
df.incurated <- df.csv[["data/speciesID/Initial_data_bats_curated.csv"]]
df.incurated.na <- df.incurated
df.incurated.na[df.incurated.na == 0] <- NA

df.incurated.long <- df.incurated.na %>% 
  pivot_longer(cols = 7:89, names_to = "SpeciesID", values_to="Pres", values_drop_na=TRUE) %>% 
  select(1:6, SpeciesID, Pres)

df.incurated.long %>% group_by(Site_name) %>% count() %>% gt()
df.incurated.fix %>% distinct(Site_name) %>% filter(str_detect(Site_name, "\\?")) %>% print(n=35)

df.incurated.fix <- df.incurated.long %>% 
  mutate(Site_name = case_when(str_detect(Site_name, "ich") ~ "Zurich",
                               str_detect(Site_name, "Bras do Regedouro") ~ "Sao Bras do Regedouro_S. Bras",
                               str_detect(Site_name, "Brissos_S. Brissos") ~ "Sao Brissos_S. Brissos",
                               .default=Site_name))

df.incurated.sep <- df.incurated.fix %>% 
  separate(Site_name, into = c("Site1", "Site2"), sep = "\\_(?!.*_)", remove = FALSE)

df.incurated.sep %>% group_by(Site1) %>% count() %>% ungroup() %>%  gt() 


df.incurated.fix$Site_name <- iconv(df.incurated.fix$Site_name , to = "UTF-8", sub = "byte")

mutate(Site_name = str_replace(Site_name, "\\?\\xa3", "a")) %>% 
  
df.incurated.fix %>% group_by(Site_name) %>% count() %>% gt()
df.incurated.fix %>% distinct(Site_name) %>% filter(str_detect(Site_name, "Sao")) %>% print(n=35)


