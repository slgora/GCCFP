#### VEG LIST project for MAARTEN ###########
### started 2025_10_23

# Load required libraries
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(stringi)
library(future.apply)
library(data.table)
library(purrr)
library(stringr)
library(tidyr)

### Filter veg list species from data
## Get data from GBIF, genebanks (Genesys), botanic gardens
## - from GCCFP complementarity project.


#-----------------------------------------#
#--- Veg list and guide files read in  ---#
#-----------------------------------------#

### 1. full veg list (~1,480 taxa)
### 2. PTFTW veg list
### 3. CWRs veg list

### 1. full veg list
## see Taxon field
veg_list <- read_excel("C:/Users/sarah/Downloads/Table S3_Extensive vegetable species list_191025.xlsx", sheet = "Species information")
# added formatted names to match output of gnverfier Standardized_taxa
# ei: hybrids using multiplication instead of "x"
# all names already standardized to WFO
veg_list <- read_excel("Outputs/Table_S3_Extensive_vegetable_species_list_191025_v2.xlsx")

# 166 taxa NOT in plant list (complementarity) that ARE in the veg list
veg_only_taxa <- read_excel("veg_only_taxa_results.xlsx")


### Primary regions of diversity guide files
## crops in primary regions file
crops_primary_regions <- read_excel("C:/Users/sarah/Downloads/FS_Items_regions_2015_4_15.xlsx")
## countries in regions file
countries_in_regions <- read_excel("C:/Users/sarah/Downloads/countries_in_regions (1).xlsx")



#----------------------#
#--- Data read in ---#
#----------------------#

## Get data from GBIF, genebanks (Genesys), botanic gardens
## - from GCCFP complementarity project.
### read in the raw files (unfiltered) with taxon name standardized

##### GENESYS DATA #####
# all data from genesys with standardized names to WFO
# 4,863,459 rows
WCFP_Genesys_data_all_standardized <- read_csv("WCFP_Genesys_data_all_standardized_2025_08_01.csv")

##### BGCI DATA #####
# all data from plant search WCFP export with standardized names to WFO
# 614,248 rows
WCFP_BGCI_data_all_standardized <- read_csv("WCFP_BGCI-PlantSearch_data_standardized_2025_08_01.csv")

###### GBIF Data #####
## only need GBIF data for 166 taxa (combine with complementarity data pull and filter)

# WCFP GBIF data
# GBIF WCFP observations data (extracted based on standardized names to WFO)
# 7,370,651 rows
# not all data has coordinates, filter out later
WCFP_GBIF_observations_data <- read_csv(
  file.path("G:/.shortcut-targets-by-id/1kQBvFIumhKQHnxSXNhr7w70dvuHgheEO/GCCFP/Data and analyses/Food plant distributions/GBIF_geo_data/WCFP_GBIF_observations_2025-08-27.csv")
) %>%
  select(latitude, longitude, genus_species, inst_code) %>%
  mutate(
    data_source = "GBIF observations",
    inst_type = "Botanic garden")

##### CANO data #####
# 386,313
WCFP_Cano_data <- read_csv("Data_processed/WCFP_Cano_data_processed_2025-08-15.csv")




#----------------------#
#--- Data processing ---#
#----------------------#

# institutions info add
WIEWS_instIDs <- read_excel("C:/Users/sarah/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/WIEWS_instIDs.xlsx")
WIEWS_instIDs_dedup <- WIEWS_instIDs %>% group_by(ORGNAME_L) %>% slice(1) %>% ungroup()

bgci_df <- WCFP_BGCI_data_all_standardized %>%
  left_join(WIEWS_instIDs_dedup, by = c("ex_situ_site_name" = "ORGNAME_L")) %>%
  select(-ID, -ORGACRO_L, -VALID_ID, -DELETED) %>%
  rename(inst_code = WIEWS_INSTCODE)

genesys_df <- WCFP_Genesys_data_all_standardized %>%
  left_join(WIEWS_instIDs, by = c("inst_code" = "WIEWS_INSTCODE")) %>%
  select(-ID, -ORGACRO_L, -VALID_ID, -DELETED) %>%
  rename(inst_name = ORGNAME_L)

# Assign institution type
bgci_df <- bgci_df %>%
  mutate(inst_type = ifelse(ex_situ_site_name %in% c("U.S. National Plant Germplasm System", "Seeds of Success (SOS)"),
                            "Genebank", "Botanic garden"))
genesys_df <- genesys_df %>%
  mutate(inst_type = ifelse(inst_code %in% c("GBR004", "USA151", "DEU502", "NLD020", "BEL014", "DEU022", "DEU515", "CHE100", "POL001", "DEU156", "CHE006", "LTU010", "ESP218", "ARM010", "POL022", "DEU078", "LVA019", "GEO002"),
                            "Botanic garden", "Genebank"))

# Remove duplicates
bgci_df <- bgci_df[!bgci_df$ex_situ_site_name %in% c("U.S. National Plant Germplasm System", "Seeds of Success (SOS)", "Millennium Seed Bank", "United States National Arboretum"), ]
genesys_df <- genesys_df[genesys_df$inst_code != "YUG001", ]

# Assign institution status
## only for Genesys
internationalgenebanks_list <- read_excel("Data/internationalgenebanks_list.xlsx")
international_codes <- internationalgenebanks_list$instCode
genesys_df <- genesys_df %>%
  mutate(inst_status = ifelse(inst_code %in% international_codes, "International", "National"))

# Assign storage type
## only for BGCI
bgci_df <- bgci_df %>%
  mutate(storage_type = paste(
    ifelse(germplasm_plant == 1, "plant", NA),
    ifelse(germplasm_seed == 1, "seed", NA),
    ifelse(germplasm_pollen == 1, "pollen", NA),
    ifelse(germplasm_explant == 1, "explant", NA),
    sep = "; "
  )) %>%
  mutate(storage_type = gsub("(^NA; |; NA$|; NA; |NA)", "", storage_type)) %>%
  mutate(storage_type = ifelse(storage_type == "", NA, storage_type))


# View semi-processed data
View(bgci_df) # 594,926
View(genesys_df) # 4,857,984
View(WCFP_GBIF_observations_data) # 7,370,651 not yet filtered to drop lat/long data NAs
View(WCFP_Cano_data) #386,313



# rename genus_species field in WCFP_GBIF_observations_data to taxa
WCFP_GBIF_observations_data <- WCFP_GBIF_observations_data %>%
  rename(taxa = genus_species)
# rename genus_species field in WCFP_Cano_data to taxa
WCFP_Cano_data <- WCFP_Cano_data %>%
  rename(taxa = genus_species)





#---------------------------#
#--- Filter for veg list ---#
#---------------------------#

# note: filter for veg list based on genus + species match of standardized taxa

# helper function to extract and normalize genus + species from standardized taxa
extract_genus_species <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("(?i)\\s*[×x]\\s*", " ") %>%
    str_squish() %>%
    str_extract("^\\S+\\s+\\S+")
}


####### Filtering Genesys for veg list taxa #######

# Step 1: Make sure there is genus_species column (normalized) from Standardized_taxa
genesys_df <- genesys_df %>%
  mutate(genus_species = extract_genus_species(Standardized_taxa))

# Step 2: Prepare genus_species terms from veg list
veg_list <- read_excel("Outputs/Table_S3_Extensive_vegetable_species_list_191025_v2.xlsx") %>%
  mutate(genus_species = extract_genus_species(Standardized_taxa))

# Clean genus-species terms from veg list
pattern_terms <- c(veg_list$genus_species) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Escape regex characters and collapse into pattern
escaped_terms <- str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1")
pattern <- str_c("(?i)", str_c(escaped_terms, collapse = "|"))

### Step 3: Optimized Filtering for large dataset
# Prepare exact match terms
exact_terms <- veg_list %>%
  select(genus_species) %>%
  pivot_longer(everything(), values_to = "genus_species") %>%
  filter(!is.na(genus_species)) %>%
  distinct()

# Filter exact matches
WCFP_Genesys_data_filtered_exact <- genesys_df %>%
  semi_join(exact_terms, by = "genus_species")

# Get remaining unmatched rows
remaining <- anti_join(genesys_df, exact_terms, by = "genus_species")

############## Convert to data.table for faster processing
library(data.table)
setDT(remaining)

# Break pattern_terms into smaller batches (e.g., 100 terms each)
term_batches <- split(pattern_terms, ceiling(seq_along(pattern_terms) / 100))

# Initialize empty list to collect matches
filtered_list <- list()

# Loop through batches and filter
for (i in seq_along(term_batches)) {
  batch <- term_batches[[i]]
  batch_pattern <- str_c("(?i)", str_c(str_replace_all(batch, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

  message("Filtering batch ", i, " of ", length(term_batches))
  filtered_chunk <- remaining[grepl(batch_pattern, remaining[["genus_species"]], ignore.case = TRUE)]
  filtered_list[[i]] <- filtered_chunk
}

# Combine all filtered results
WCFP_Genesys_data_filtered_regex <- rbindlist(filtered_list)

# Combine with exact matches
WCFP_Genesys_data_filtered <- bind_rows(WCFP_Genesys_data_filtered_exact, WCFP_Genesys_data_filtered_regex)

#save
write.csv(WCFP_Genesys_data_filtered, 'Agrobio_veg_list/Data/Processed/WCFP_Genesys_data_filtered_veglist_2025-10-24.csv', row.names = FALSE)






####### Filtering BGCI for veg list taxa #######

# Step 1: Make sure there is genus_species column (normalized) from Standardized_taxa
bgci_df <- bgci_df %>%
  mutate(genus_species = extract_genus_species(Standardized_taxa))

# Clean genus_species terms from veg list
pattern_terms <- c(veg_list$genus_species) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Create regex pattern
pattern <- str_c("(?i)", str_c(str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

# Step 2: Filter all matching rows OR if synonym in BGCI
WCFP_BGCI_filtered <- bgci_df %>%
  filter(
    str_detect(genus_species, regex(pattern, ignore_case = TRUE)) |
      taxon_name_status == "Synonym")
# Save
write.csv(WCFP_BGCI_filtered, 'Agrobio_veg_list/Data/Processed/WCFP_BGCI_data_filtered_veglist_2025-10-24.csv', row.names = FALSE)







####### Filtering GBIF for veg list taxa #######


# Step 1: Make sure there is genus_species column (normalized) from Standardized_taxa
WCFP_GBIF_observations_data <- WCFP_GBIF_observations_data %>%
  mutate(genus_species = extract_genus_species(taxa))

# Step 2: Prepare genus-species terms from veg list
veg_list <- read_excel("Outputs/Table_S3_Extensive_vegetable_species_list_191025_v2.xlsx") %>%
  mutate(genus_species = extract_genus_species(Standardized_taxa))

# Clean genus-species terms from veg list
pattern_terms <- c(veg_list$genus_species) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Escape regex characters and collapse into pattern
escaped_terms <- str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1")
pattern <- str_c("(?i)", str_c(escaped_terms, collapse = "|"))

### Step 3: Optimized Filtering for large dataset
# Prepare exact match terms
exact_terms <- veg_list %>%
  select(genus_species) %>%
  pivot_longer(everything(), values_to = "genus_species") %>%
  filter(!is.na(genus_species)) %>%
  distinct()

# Filter exact matches
WCFP_GBIF_observations_data_filtered_exact <- WCFP_GBIF_observations_data %>%
  semi_join(exact_terms, by = "genus_species")

# Get remaining unmatched rows
remaining <- anti_join(WCFP_GBIF_observations_data, exact_terms, by = "genus_species")

############## Convert to data.table for faster processing
library(data.table)
setDT(remaining)

# Break pattern_terms into smaller batches (e.g., 100 terms each)
term_batches <- split(pattern_terms, ceiling(seq_along(pattern_terms) / 100))

# Initialize empty list to collect matches
filtered_list <- list()

# Loop through batches and filter
for (i in seq_along(term_batches)) {
  batch <- term_batches[[i]]
  batch_pattern <- str_c("(?i)", str_c(str_replace_all(batch, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

  message("Filtering batch ", i, " of ", length(term_batches))
  filtered_chunk <- remaining[grepl(batch_pattern, remaining[["genus_species"]], ignore.case = TRUE)]
  filtered_list[[i]] <- filtered_chunk
}

# Combine all filtered results
WCFP_GBIF_observations_data_filtered_regex <- rbindlist(filtered_list)

# Combine with exact matches
WCFP_GBIF_observations_data_filtered <- bind_rows(WCFP_GBIF_observations_data_filtered_exact, WCFP_GBIF_observations_data_filtered_regex)

#save
write.csv(WCFP_GBIF_observations_data_filtered, 'Agrobio_veg_list/Data/Processed/WCFP_GBIF_observations_data_filtered_veglist__2025-10-24.csv', row.names = FALSE)






####### Filtering Cano et al for veg list taxa #######

# Step 1: Make sure there is genus_species column (normalized) from Standardized_taxa
# taxa already standardized to WFO from Cano, double check this
WCFP_Cano_data <- WCFP_Cano_data %>%
  mutate(genus_species = extract_genus_species(taxa))

# Clean genus_species terms from veg list
pattern_terms <- c(veg_list$genus_species) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Create regex pattern
pattern <- str_c("(?i)", str_c(str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

# Step 2: Filter all matching rows
WCFP_Cano_filtered <- WCFP_Cano_data %>%
  filter(str_detect(genus_species, regex(pattern)))

# Save
write.csv(WCFP_Cano_filtered, 'Agrobio_veg_list/Data/Processed/WCFP_Cano_data_filtered_veglist_2025-10-24.csv', row.names = FALSE)






#####################################################################################
##### Show what taxa from veglist are NOT found in each dataset #####################

# required packages (install if needed)
library(dplyr)
library(purrr)
library(stringr)
library(writexl)

# ---- helper: normalize/extract a character vector from a column (handles list-columns) ----
norm_chr <- function(x) {
  # x can be character/factor or list-column
  if (is.list(x)) {
    # take first element of each list entry (change collapse rule if you prefer)
    v <- map_chr(x, ~ {
      if (is.null(.x) || length(.x) == 0) return(NA_character_)
      as.character(.x)[1]
    })
  } else {
    v <- as.character(x)
  }
  # trim whitespace and convert empty strings to NA, then lowercase for case-insensitive matching
  v <- trimws(v)
  v[v == ""] <- NA_character_
  v_lower <- tolower(v)
  v_lower
}

# ---- prepare unique taxa from veg_list ----
# keep an original-display version (trimmed, first-appearance) and a normalized version for matching
veg_raw <- if (is.list(veg_list$genus_species)) {
  purrr::map_chr(veg_list$genus_species, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x)[1])
} else {
  as.character(veg_list$genus_species)
}
veg_raw <- trimws(veg_raw)
veg_raw[veg_raw == ""] <- NA_character_

# unique display taxa (preserve original casing)
display_taxa <- unique(na.omit(veg_raw))

# normalized taxa for matching
veg_norm <- tolower(display_taxa)

# ---- list the datasets you want to check (replace these names with your actual data.frame names) ----
datasets <- list(
  Genesys = WCFP_Genesys_data_filtered,
  BGCI = WCFP_BGCI_filtered,
  GBIF = WCFP_GBIF_observations_data_filtered,
  Cano = WCFP_Cano_filtered
)

# ---- build presence/absence columns ----
result <- tibble(taxa = display_taxa)

for (ds_name in names(datasets)) {
  df <- datasets[[ds_name]]
  if (!"genus_species" %in% names(df)) stop(glue::glue("Dataset {ds_name} has no column 'genus_species'"))
  present_set <- unique(na.omit(norm_chr(df$genus_species)))   # normalized present taxa in dataset
  present_logical <- veg_norm %in% present_set
  # map to "Y"/"N"
  result[[ds_name]] <- ifelse(present_logical, "Y", "N")
}

# ---- optionally: filter to only taxa NOT found in ANY dataset ----
# set this flag to TRUE if you want to output only taxa that are absent from all datasets
only_not_found_in_any <- FALSE

if (only_not_found_in_any) {
  # find rows where all dataset columns are "N"
  dataset_cols <- names(datasets)
  result <- result %>% filter(if_all(all_of(dataset_cols), ~ .x == "N"))
}

# ---- write to Excel ----
out_file <- "veg_list_presence_by_dataset.xlsx"
writexl::write_xlsx(result, path = out_file)





# ---- optionally: filter to only taxa NOT found in ANY dataset ----
# set this flag to TRUE if you want to output only taxa that are absent from all datasets
only_not_found_in_any <- TRUE

if (only_not_found_in_any) {
  # find rows where all dataset columns are "N"
  dataset_cols <- names(datasets)
  result <- result %>% filter(if_all(all_of(dataset_cols), ~ .x == "N"))
}

# ---- write to Excel ----
out_file <- "veg_list_presence_by_dataset2.xlsx"
writexl::write_xlsx(result, path = out_file)











###################### data exploration, counts ##################
# dataset names
dataset_names <- c(
  "WCFP_Genesys_data_filtered",
  "WCFP_BGCI_filtered",
  "WCFP_GBIF_observations_data_filtered",
  "WCFP_Cano_filtered"
)

# helper: normalize/extract a character vector from a column (handles list-columns)
norm_col <- function(x) {
  if (is.list(x)) {
    v <- purrr::map_chr(x, ~ {
      if (is.null(.x) || length(.x) == 0) return(NA_character_)
      as.character(.x)[1]
    })
  } else {
    v <- as.character(x)
  }
  v <- trimws(v)
  v[v == ""] <- NA_character_
  v_lower <- tolower(v)
  v_lower
}

# compute counts and collect unique taxa per dataset
results <- map_dfr(dataset_names, function(obj_name) {
  if (!exists(obj_name, envir = .GlobalEnv)) {
    return(tibble(dataset = obj_name, rows = NA_integer_, unique_taxa = NA_integer_, taxa_col = NA_character_, note = "object not found"))
  }
  df <- get(obj_name, envir = .GlobalEnv)
  rows <- if (is.data.frame(df) || is.matrix(df)) nrow(df) else NA_integer_
  taxa_col <- if ("genus_species" %in% names(df)) "genus_species" else NA_character_
  unique_taxa <- NA_integer_
  note <- ""
  if (!is.na(taxa_col)) {
    vec <- norm_col(df[[taxa_col]])
    unique_taxa <- length(unique(na.omit(vec)))
  } else {
    note <- "no genus_species column"
  }
  tibble(dataset = obj_name, rows = rows, unique_taxa = unique_taxa, taxa_col = taxa_col, note = note)
})

# create a list of data.frames to write to Excel:
# first sheet: counts; following sheets: unique taxa for each dataset
sheets <- list(Counts = results)

for (ds in dataset_names) {
  if (!exists(ds, envir = .GlobalEnv)) {
    sheets[[ds]] <- tibble(message = paste("Dataset", ds, "not found in Global Environment"))
  } else {
    df <- get(ds, envir = .GlobalEnv)
    if (!"genus_species" %in% names(df)) {
      sheets[[ds]] <- tibble(message = paste("No genus_species column in", ds))
    } else {
      taxa_vec <- norm_col(df$genus_species)
      taxa_unique <- sort(unique(na.omit(taxa_vec)))
      # restore display column name and keep original-cased values? here we save normalized lower-case
      sheets[[ds]] <- tibble(genus_species = taxa_unique)
    }
  }
}

# sanitize and make sheet names unique and <= 31 chars
clean_and_unique_sheet_names <- function(names_vec) {
  badchars <- "[\\\\/:?*\\[\\]]"
  cleaned <- gsub(badchars, "", names_vec)
  cleaned <- trimws(cleaned)
  cleaned[cleaned == ""] <- "Sheet"
  res <- character(length(cleaned))
  used <- character()
  for (i in seq_along(cleaned)) {
    base_raw <- cleaned[i]
    # initial truncate to 31
    base <- substr(base_raw, 1, 31)
    candidate <- base
    j <- 1
    while (candidate %in% used) {
      suffix <- paste0("_", j)
      max_base_len <- 31 - nchar(suffix)
      if (max_base_len < 1) max_base_len <- 1
      candidate <- paste0(substr(base, 1, max_base_len), suffix)
      j <- j + 1
    }
    res[i] <- candidate
    used <- c(used, candidate)
  }
  res
}

# assume `sheets` and `out_file` exist as in your prior code
orig_names <- names(sheets)
new_names <- clean_and_unique_sheet_names(orig_names)

# add a mapping sheet so you can see original -> new
mapping_df <- data.frame(original_name = orig_names, sheet_name = new_names, stringsAsFactors = FALSE)

# apply new names and append mapping sheet
names(sheets) <- new_names
sheets[["Sheet_Name_Map"]] <- mapping_df

# write workbook
openxlsx::write.xlsx(sheets, file = out_file, asTable = TRUE)

cat("Wrote", out_file, "with sanitized sheet names. Mapping sheet: Sheet_Name_Map\n")


