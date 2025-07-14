### Project: Agrobiodiversity ###
### Data exploration
### Agrobiodiversity_dataexploration_wrangling.R
### by Sarah Gora
### Date created: 2025_03_26
### Updated: 2025_06_24

library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(httr)
library(jsonlite)
library(stringr)

################# Plant List Read In #################
WCFP_plantlist <- read_excel("C:/Users/sarah/Desktop/Agrobiodiversity/GCCFP/Plants_list/WCFP_simplified_240921.xlsx")

################# Genesys Data Read In #########################################
# read in Genesys data export (4,863,459 entries, 19 total columns, all of genesys accessions)
WCFP_Genesys_data_all <- read.csv("Data/colin_dataset_from_Christelle_2025-03-25.csv", sep = ";")
WCFP_Genesys_data_all <- cbind(WCFP_Genesys_data_all, data_source = "Genesys") # Add field: data_source

################## BGCI Plant Search Data Read In #############################
# read in BGCI Plant Search export data and combine into one file (621,651 entries, 23 total columns)
file_paths <- list.files("Data/exports from PS", full.names = TRUE, pattern = "\\.csv$")
WCFP_BGCI_data <- file_paths %>% map_df(read.csv)
WCFP_BGCI_data <- cbind(WCFP_BGCI_data, data_source = "BGCI") # Add field: data_source


############## Align Genesys and BGCI PS taxon to WFO ###########################

# Import functions for taxa standardization
source("Functions/query_taxa_resolver.R") # function to query API of https://verifier.globalnames.org
source("Functions/extract_best_result.R") # function to extract best results from the query search

# For Genesys, use intraspecific data in SUBTAXA field to standardize taxa
# make a new column called "taxa" by combining GENUS, SPECIES, and SUBTAXA (but still keeps original columns)
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(taxa = paste(GENUS, SPECIES, SUBTAXA, sep = " "))

library(stringr)
install.packages("future.apply")
library(future.apply)  # For parallelization
# Function to clean and standardize a taxonomic string
clean_taxonomic_string <- function(taxon_string) {
  if (is.na(taxon_string) || trimws(taxon_string) == "") return(NA)

  # Remove brackets and special annotations
  taxon_string <- str_replace_all(taxon_string, "\\[|\\]", "")
  taxon_string <- str_replace_all(taxon_string, "ERSP\\+FERR\\+MILT\\+LUT", "")
  taxon_string <- str_replace_all(taxon_string, "'paul no\\?l'", "")
  taxon_string <- str_replace_all(taxon_string, "\\?", " ")

  # Apply replacements
  replacements <- c(
    "dardarii" = "dardari",
    "var\\. nutans\\+defic" = "var. nutans",
    "convar\\. dentiformis\\+mays" = "convar. dentiformis",
    "convar\\. semident \\(dentiformis\\+mays\\)" = "convar. semindent",
    "var\\. roshanum \\(uzunsov\\)\\+ rufulinflatum" = "var. roshanum Uzunsov",
    "var\\. wernerianum\\+karsiense" = "var. wernerianum; var. karsiense",
    "var\\. echinoides\\+ag dənli" = "var. echinoides",
    "sp\\. Paluma Range \\(G\\. Sankowsky\\+ 450\\)" = "sp.",
    "sp\\. Mt Isa \\(R\\.L\\. Specht \\+ 49\\)" = "sp.",
    "nut\\+pall" = "subsp. nutans and subsp. pallidum",
    "r/lutescens\\+lutences" = "var. lutescens",
    "var\\. erythrospermum\\+ferr" = "var. erythrospermum",
    "montana\\+brunnea/a\\.vaviloviana/" = "(montana, brunnea); vaviloviana"
  )

  for (pattern in names(replacements)) {
    taxon_string <- str_replace_all(taxon_string, pattern, replacements[pattern])
  }

  # Break on delimiters and clean
  parts <- unlist(strsplit(taxon_string, "[\\+/\\(\\)/]"))
  parts <- trimws(tolower(parts))
  parts <- parts[parts != ""]

  # Capitalize genus and clean known names
  parts <- gsub("^a\\.?\\s*vaviloviana$", "Avena vaviloviana", parts)
  parts <- gsub("^avena", "Avena", parts)
  parts <- gsub("^triticum", "Triticum", parts)
  parts <- gsub("^zea", "Zea", parts)
  parts <- gsub("^hordeum", "Hordeum", parts)
  parts <- gsub("^cassia", "Cassia", parts)
  parts <- gsub("^brassica", "Brassica", parts)

  parts <- sapply(parts, function(p) {
    words <- strsplit(p, "\\s+")[[1]]
    words[1] <- paste0(toupper(substring(words[1], 1, 1)), substring(words[1], 2))
    paste(words, collapse = " ")
  })

  paste(unique(parts), collapse = "; ")
}

library(stringr)

# 🎯 Target columns
target_cols <- c("SPECIES", "SPAUTHOR", "SUBTAXA")

# 📦 Chunk configuration
chunk_size <- 500000
total_rows <- nrow(WCFP_Genesys_data_all)
num_chunks <- ceiling(total_rows / chunk_size)

# Chunked processing loop
for (col_name in target_cols) {
  message("🚀 Starting cleaning for column: ", col_name)

  for (i in seq_len(num_chunks)) {
    start_row <- (i - 1) * chunk_size + 1
    end_row <- min(i * chunk_size, total_rows)

    message("⏳ Processing rows ", start_row, " to ", end_row, " of ", total_rows)

    # Time the chunk
    chunk_time <- system.time({
      WCFP_Genesys_data_all[start_row:end_row, col_name] <- vapply(
        WCFP_Genesys_data_all[start_row:end_row, col_name],
        clean_taxonomic_string,
        FUN.VALUE = character(1)
      )
    })

    message("✅ Finished chunk ", i, "/", num_chunks, " in ",
            round(chunk_time[3], 2), " seconds")

    # Optional: Save interim results
    # saveRDS(WCFP_Genesys_data_all, paste0("cleaned_", col_name, "_chunk_", i, ".rds"))
  }

  message("🎉 All chunks done for column: ", col_name)
}







# Fix some taxa names mispelled in Genesys
delete "[" and "]"
delete "ERSP+FERR+MILT+LUT"
delete "'paul no?l'"
replace "dardarii" with "dardari"
replace "?" with " "
replace "var. nutans+defic" with "var. nutans"
replace "convar. dentiformis+mays" with "convar. dentiformis"
replace "convar. semident (dentiformis+mays)" with "convar. semindent"
replace "var.roshanum (uzunsov)+ rufulinflatum" with "var. roshanum Uzunsov"
replace "var.roshanum (uzunsov)+ rufulinflatum" with "var. wernerianum; var. karsiense"
replace "var.echinoides+ag dənli" with "var. echinoides"
replace "sp. Paluma Range (G. Sankowsky+ 450)" with "sp."
replace "sp. Mt Isa (R.L. Specht + 49)" with "sp."
replace "nut+pall" with "subsp. nutans and subsp. pallidum"
replace "r/lutescens+lutences" with "var. lutescens"
replace "var. erythrospermum+ferr" with "var. erythrospermum"
replace "montana+brunnea/a.vaviloviana/" with "(montana, brunnea); vaviloviana"




# load Genesys Data and Functions
df <- WCFP_Genesys_data_all
# Prepare Taxa List
taxa_list <- unique(trimws(na.omit(df$taxa)))

# Query API for WFO Standardization using query_taxa_resolver function
result_queries_WFO <- list()
counter <- 0
for (i in taxa_list){
  print(paste(round(counter / length(taxa_list) * 100, 2), "%", i))
  best_result_WFO <- query_taxa_resolver(i, c('196'))  # Run against WFO
  result_queries_WFO <- append(result_queries_WFO, list(best_result_WFO))
  counter <- counter + 1
}

# Extract Best Matches using extract_best_results function
res_WFO <- extract_best_result(result_queries_WFO)

# Create a Taxa Dictionary
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')

# Save WFO Taxa Dictionary for Genesys
df_save_results <- apply(taxa_standardized_df_WFO, 2, as.character)
write.csv(df_save_results, 'Data/Processing/Run_2025_07_11/WCFP_Genesys_standardized_taxa_WFO2025_07_11.csv', row.names = FALSE)


#### Add standardized_taxa column to Genesys dataset
# read in Taxa Dictionary (if picking up from here)
standardization_table_Genesys <- read_csv("Data/Processing/Run_2025_07_11/WCFP_Genesys_standardized_taxa_WFO2025_07_11.csv")
# Structure standardization table for easy look up
standardization_table_Genesys <- as.data.frame(standardization_table_Genesys, stringsAsFactors = FALSE)
standardization_table_Genesys <- setNames(standardization_table_Genesys$output_name_WFO, standardization_table_Genesys$input_name)
# example name look up
standardization_table_Genesys["Avena sativa"]

# Make Standardized_taxa column
WCFP_Genesys_data_all$Standardized_taxa <- NA
# Trim whitespace in taxa field
WCFP_Genesys_data_all$taxa <- trimws(WCFP_Genesys_data_all$taxa)
# Add standardized taxa field to Genesys dataset
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(Standardized_taxa = standardization_table_Genesys[match(taxa, names(standardization_table_Genesys))])



############## what to do about taxa that were not standardized, send to CK #########
# check unmatched taxa, 83
unmatched_taxa <- setdiff(unique(WCFP_Genesys_data_all$taxa), names(standardization_table_Genesys))

# add column if taxa was matched T/F
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(Match_Found = taxa %in% names(standardization_table_Genesys))
# view taxa not standardized
Genesys_unmatched_df <- WCFP_Genesys_data_all %>%
  filter(!Match_Found)

####################################################################################


# Standardize Genesys field names
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  rename(
    uuid = UUID,
    historic = HISTORIC,
    inst_code = INSTCODE,
    acce_numb = ACCENUMB,
    doi = DOI,
    genus = GENUS,
    species = SPECIES,
    sp_author = SPAUTHOR,
    sub_taxa = SUBTAXA,
    sub_t_author = SUBTAUTHOR,
    samp_stat = SAMPSTAT,
    storage = STORAGE,
    orig_cty = ORIGCTY,
    latitude = DECLATITUDE,
    longitude = DECLONGITUDE,
    taxon_name_submitted = taxa,
    taxon_name_standardized_WFO = Standardized_taxa)

## Filter for our crops/list from plant list
# Keep rows in WCRP_Genesys_data_all where the taxa name (taxon_name_accepted) in WCFP_plantlist
# matches the standardized taxa names (taxon_name_standardized_WFO) in WCRP_Genesys_data_all
# Note: doesnt have to be exact match, just match genus + species
# collapse the plant list into a regex pattern, removing any special characters
pattern <- str_c(
  str_replace_all(WCFP_plantlist$taxon_name_accepted, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"),
  collapse = "|")
WCFP_Genesys_data_filtered <- WCFP_Genesys_data_all %>%  #result: 3.3 million rows
  filter(str_detect(taxon_name_standardized_WFO, pattern))


filtered_mismatches <- WCFP_Genesys_data_all %>%
  filter(Match_Found == FALSE)



############## Align BGCI Plant Search taxon to WFO ###########################

# Load Data & Functions
df <- WCFP_BGCI_data
# Prepare Taxa List
taxa_list <- unique(trimws(na.omit(df$Accepted.Name..in.PlantSearch.)))
# Query API for WFO Standardization
result_queries_WFO <- list()
counter <- 0
for (i in taxa_list){
  print(paste(round(counter / length(taxa_list) * 100, 2), "%", i))
  best_result_WFO <- query_taxa_resolver(i, c('196'))  # Run against WFO
  result_queries_WFO <- append(result_queries_WFO, list(best_result_WFO))
  counter <- counter + 1
}
# Extract Best Matches
res_WFO <- extract_best_result(result_queries_WFO)
# Create a Taxa Dictionary
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
# Save Results
# df_save_results <- apply(taxa_standardized_df_WFO, 2, as.character)
# write.csv(df_save_results, 'WCFP_BGCI-PlantSearch_standardized_taxa_WFO_06_03_25.csv', row.names = FALSE)


# run on Synonyms too
taxa_list2 <- unique(trimws(na.omit(df$Synonymous.Name..in.PlantSearch.)))
# Query API for WFO Standardization
result_queries_WFO <- list()
counter <- 0
for (i in taxa_list){
  print(paste(round(counter / length(taxa_list) * 100, 2), "%", i))
  best_result_WFO <- query_taxa_resolver(i, c('196'))  # Run against WFO
  result_queries_WFO <- append(result_queries_WFO, list(best_result_WFO))
  counter <- counter + 1
}
# Extract Best Matches
res_WFO <- extract_best_result(result_queries_WFO)
# Create a Taxa Dictionary
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
# Save Results
df_save_results <- apply(taxa_standardized_df_WFO, 2, as.character)
write.csv(df_save_results, 'WCFP_BGCI-PlantSearch_standardized_syntaxa_WFO_06_25_25.csv', row.names = FALSE)




#### add standardized_taxa column
# read in
WCFP_BGCI_PlantSearch_standardized_taxa <- read_csv("Data/WCFP_BGCI-PlantSearch_standardized_taxa_WFO_06_03_25.csv")
# standardization table is correctly structured
standardization_table_BGCI <- setNames(WCFP_BGCI_PlantSearch_standardized_taxa$output_name_WFO, WCFP_BGCI_PlantSearch_standardized_taxa$input_name)
# WCFP_BGCIPlantSearch_data_all has the Standardized_taxa column initialized
WCFP_BGCI_data$Standardized_taxa <- NA
# Add standardized names from WFO to WCFP_BGCIPlantSearch_data
WCFP_BGCIPlantSearch_data2 <- WCFP_BGCI_data%>%
  mutate(Standardized_taxa = ifelse(!is.na(Accepted.Name..in.PlantSearch.) & Accepted.Name..in.PlantSearch. %in% names(standardization_table_BGCI),
                                    standardization_table_BGCI[Accepted.Name..in.PlantSearch.],
                                    Standardized_taxa))

#### add standardized_syntaxa column
# read in
WCFP_BGCI_PlantSearch_standardized_syntaxa <- read_csv("WCFP_BGCI-PlantSearch_standardized_syntaxa_WFO_06_25_25.csv")
# standardization table is correctly structured
syn_standardization_table_BGCI <- setNames(WCFP_BGCI_PlantSearch_standardized_syntaxa$output_name_WFO, WCFP_BGCI_PlantSearch_standardized_syntaxa$input_name)
# WCFP_Genesys_data_all has the Standardized_syntaxa column initialized
WCFP_BGCIPlantSearch_data2$Standardized_syntaxa <- NA
# Add standardized syn names from WFO to WCFP_BGCIPlantSearch_data
WCFP_BGCIPlantSearch_data2 <- WCFP_BGCIPlantSearch_data2 %>%
  mutate(Standardized_syntaxa = ifelse(!is.na(Synonymous.Name..in.PlantSearch.) & Synonymous.Name..in.PlantSearch. %in% names(syn_standardization_table_BGCI),
                                    syn_standardization_table_BGCI[Synonymous.Name..in.PlantSearch.],
                                    Standardized_syntaxa))

# combine Standardized_syntaxa with Standardized_taxa field (there is no overlap)
WCFP_BGCIPlantSearch_data2 <- WCFP_BGCIPlantSearch_data2 %>%
  mutate(Standardized_taxa = coalesce(Standardized_taxa, Standardized_syntaxa)) %>%
  select(-Standardized_syntaxa)

# drop all rows where Accepted.Name..in.PlantSearch.=NA AND synonymous.Name..in.PlantSearch=NA
WCFP_BGCI_data3 <- WCFP_BGCIPlantSearch_data2 %>%
  mutate(
    accepted = na_if(`Accepted.Name..in.PlantSearch.`, ""), # helped columns, had trouble reading names
    synonym = na_if(`Synonymous.Name..in.PlantSearch.`, "")
  ) %>%
  filter(!(is.na(accepted) & is.na(synonym))) %>%
  select(-accepted, -synonym)  # delete helped columns
# 7,403 rows dropped that were NAs in accepted name and synonym name in the initial search of BGCI

# save
# write.csv(WCFP_BGCIPlantSearch_data2, 'WCFP_BGCI_PlantSearch_standardized_taxa_WFO_df_06_06_25.csv', row.names = FALSE)



# Standardize BGCI field names
# read in
# WCFP_BGCI_data_all3 <- read_csv("Data/WCFP_BGCI_PlantSearch_standardized_taxa_WFO_df_06_06_25.csv")
# Standardize BGCI column names
WCFP_BGCI_data3 <- WCFP_BGCI_data_all3 %>%
  rename(
    taxon_name_submitted = Submitted.Name,
    taxon_name_accepted_PlantSearch = Accepted.Name..in.PlantSearch.,
    taxon_synonymous_name_PlantSearch = Synonymous.Name..in.PlantSearch.,
    added_submitted_names = Added.to.Submitted.Names.,
    taxon_name_status = Name.Status,
    plantsearch_id = PlantSearch.ID,
    cultivar = Cultivar,
    ex_situ_garden_id = Ex.Situ.Site.GardenSearch.ID,
    ex_situ_site_name = Ex.Situ.Site.Name,
    city = City,
    state_province = State.or.Province,
    country = Country,
    country_code = Country.Code,
    latitude = Latitude,
    longitude = Longitude,
    germplasm_plant = Germplasm..plant,
    germplasm_seed = Germplasm..seed,
    germplasm_pollen = Germplasm..pollen,
    germplasm_explant = Germplasm..explant,
    taxon_name_standardized_WFO = Standardized_taxa)

## Filter for our crops/list from plant list
# filter for WCFP crops
# (1) Keep rows in WCRP_BGCI_data_all3 where the taxa name (taxon_name_accepted)
# in WCFP_plantlist matches the standardized taxa names (taxon_name_standardized_WFO) in WCRP_BGCI_data_all3
# (2) if there is an NA in the taxon_name_standardized_WFO then keep rows if taxa name (taxon_name_accepted)
# in WCFP_plantlist matches the taxon name submitted (taxon_name_standardized_WFO) in WCRP_BGCI_data_all3
# (3) or if NA in standardized name then if WCFP_plantlist matches the taxon synonym (taxon_synonymous_name_PlantSearch) in WCRP_BGCI_data_all3
# (4) or keep rows directly where taxon_name_submitted matches taxon_name_accepted, even if taxon_name_standardized_WFO is not NA.

## redo for partial match
# Clean and normalize plant list
extract_genus_species <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("(?i)\\s*[×x]\\s*", " ") %>%  # normalize hybrid marker
    str_squish() %>%
    str_extract("^\\S+\\s+\\S+")  # grab only first two words
}

WCFP_plantlist <- WCFP_plantlist %>%
  mutate(genus_species = extract_genus_species(taxon_name_accepted))

WCFP_BGCI_data3_cleaned <- WCFP_BGCI_data3 %>%
  mutate(genus_species = extract_genus_species(taxon_name_standardized_WFO))

WCFP_BGCI_data3_cleaned <- WCFP_BGCI_data3_cleaned %>%
  mutate(Match_Found = genus_species %in% WCFP_plantlist$genus_species)

WCFP_BGCI_dropped_rows <- WCFP_BGCI_data3_cleaned %>%  # 101,650 rows, the cleaned taxa does not match
  filter(!Match_Found)


# write down Qs about what to do about matches filtering out stage








######## Fill out institutions fields ############
# read in WIEWS inst codes file
WIEWS_instIDs <- read_excel("C:/Users/sarah/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/WIEWS_instIDs.xlsx")

# BGCI has ex_sit_site_name, assign inst_code via ex situ garden site name?
bgci_df <- WCFP_BGCI_data3_cleaned #temp
# Keep only the first match in WIEWS_instIDs per ORGNAME_L
WIEWS_instIDs_dedup <- WIEWS_instIDs %>%
  group_by(ORGNAME_L) %>%
  slice(1) %>%
  ungroup()
# Perform the join with the deduplicated table
bgci_df <- bgci_df %>%
  left_join(WIEWS_instIDs_dedup, by = c("ex_situ_site_name" = "ORGNAME_L")) %>%
  select(-ID, -ORGACRO_L, -VALID_ID, -DELETED) %>%
  rename(inst_code = WIEWS_INSTCODE)
# Genesys has inst_code, fill out the inst_name field
genesys_df <- WCFP_Genesys_data_filtered #temp
genesys_df <- genesys_df %>%
  left_join(WIEWS_instIDs, by = c("inst_code" = "WIEWS_INSTCODE")) %>%
  select(-ID, -ORGACRO_L, -VALID_ID, -DELETED) %>%
  rename(inst_name = ORGNAME_L)


############### Assign institution type ##############
# Assign inst_type for BGCI as Botanic Garden, except for special cases
bgci_df$inst_type <- ifelse(bgci_df$ex_situ_site_name %in% c("U.S. National Plant Germplasm System",
                                                                  "Seeds of Success (SOS)"),
                                 "Genebank",
                                 "Botanic garden")
# Assign inst_type for Genesys as Genebank, except for special cases
genesys_df$inst_type <- ifelse(genesys_df$inst_code %in% c("GBR004", "USA151", "DEU502", "NLD020", "BEL014",
                                                              "DEU022", "DEU515", "CHE100", "POL001", "DEU156",
                                                              "CHE006", "LTU010", "ESP218", "ARM010", "POL022",
                                                              "DEU078", "LVA019", "GEO002"),
                                   "Botanic garden",
                                   "Genebank")

############### Remove duplicates ###############
# Remove duplicates from BGCI (based on CK notes in guide file), keep in genesys
bgci_df <- bgci_df[!bgci_df$ex_situ_site_name %in% c("U.S. National Plant Germplasm System",
                                               "Seeds of Success (SOS)",
                                               "Millennium Seed Bank",
                                               "United States National Arboretum"), ]
# Remove duplicate genebank in Genesys YUG001 (already counted in SRB001 in Genesys)
genesys_df <- genesys_df[genesys_df$inst_code != "YUG001", ]


############### Assign Institution Status ###############
# assign international genebanks based on inst_code
# genesys only?
# are there any botanic gardens international organizations?
# international genebanks list implements international organizations from FAO WIEWS inst list
internationalgenebanks_list <- read_excel("Data/internationalgenebanks_list.xlsx")
# Create a list of international instCodes
international_codes <- internationalgenebanks_list$instCode
# Assign inst_status in genesys_df
genesys_df$inst_status <- ifelse(genesys_df$inst_code %in% international_codes,
                                 "International",
                                 "National")







### to do: Annotate the Plant List
# read in plant list
WCFP_plantlist <- read_excel("C:/Users/sarah/Desktop/Agrobiodiversity/GCCFP/Plants_list/WCFP_simplified_240921.xlsx")
##### 1. major crops vs minor crops
## anything NOT on ptftw list as minor crop
##### 2. Cultivated vs wild? - how to easily do?
##### 3. Food group (cereal, pulse, vegetable, etc.)?
















# View and explore data, counts 06_25_2025

# view the accepted taxa names in BGCI
unique_taxa_df <- data.frame(taxon_name_accepted_PlantSearch = unique(WCFP_BGCI_data_filtered$taxon_name_accepted_PlantSearch))
# count the # of unique taxa in BGCI, taxon_name_accepted_PlantSearch
unique_taxa_count <- length(unique(WCFP_BGCI_data_filtered$taxon_name_accepted_PlantSearch[!is.na(WCFP_BGCI_data_filtered$taxon_name_accepted_PlantSearch)]))
# 17296
# count the # of unique taxa in BGCI, taxon_name_standardized_WFO
unique_taxa_count <- length(unique(WCFP_BGCI_data_filtered$taxon_name_standardized_WFO[!is.na(WCFP_BGCI_data_filtered$taxon_name_standardized_WFO)]))
# 17255




# view the accepted taxa names in Genesys
unique_taxa_df <- data.frame(taxon_name_submitted = unique(WCFP_Genesys_data_filtered$taxon_name_submitted))
# count the # of unique taxa in Genesys, taxon_name_submitted
unique_taxa_count <- length(unique(WCFP_Genesys_data_filtered$taxon_name_submitted[!is.na(WCFP_Genesys_data_filtered$taxon_name_submitted)]))
# 26254
# count the # of unique taxa in Genesys, taxon_name_standardized_WFO
unique_taxa_count <- length(unique(WCFP_Genesys_data_filtered$taxon_name_standardized_WFO[!is.na(WCFP_Genesys_data_filtered$taxon_name_standardized_WFO)]))
# 13,918



# count of the number of accessions in institutions
# not a metric, just to view the top instituions holding germplasm

# add institution names to genesys for easier review
WIEWS_instIDs <- read_excel("C:/Users/sarah/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/WIEWS_instIDs.xlsx")
gen_summary_by_instcode <- gen_summary_by_instcode %>%
  left_join(WIEWS_instIDs, by = c("inst_code" = "WIEWS_INSTCODE")) %>%
  select(-ID, -ORGACRO_L, -VALID_ID, -DELETED) %>%
  rename(org_name = ORGNAME_L)

gen_summary_by_instcode <- WCFP_Genesys_data_filtered %>% # Genesys
  count(inst_code, name = "n_rows") %>%
  rename(accessions_count = n_rows) %>%
  arrange(desc(accessions_count))

bgci_summary_by_site <- WCFP_BGCI_data_filtered %>%  # BGCI
  count(ex_situ_site_name, name = "n_rows") %>%
  rename(accessions_count = n_rows) %>%
  arrange(desc(accessions_count))
