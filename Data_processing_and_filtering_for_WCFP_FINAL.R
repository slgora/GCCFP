### Project: Agrobiodiversity, GCCFP, WCFP Complementarity ###
### Data cleaning, processing and filtering for new WCFP plantlist data
### Script by Sarah Gora
### Date created: 2026_02_23

# Load necessary packages
library(readr)
library(readxl)
library(writexl)
library(dplyr)
library(purrr)
library(tidyr)
library(httr)
library(jsonlite)
library(stringr)
library(stringi)
library(future.apply)
library(data.table)

#----------------------#
#--- Helper Functions -#
#----------------------#

# Function to normalize genus + species
source("Functions/extract_genus_species.R")


#-----------------------------------------------------#
#---- 2026 WCFP PLantlist Read-In: Standardized -----#
#-----------------------------------------------------#

# WCFP PLANT LIST (CORRECTED by SG 2026-03-05): standardized
WCFP_plantlist_stand <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/WCFP_plantlist/Standardized/WCFP_plantlist_standardized_2026-02-23_updated.xlsx")


#---------------------------------------------------------------#
#--- 2026 Data Read-In: Standardized, unfiltered for WCFP -----#
#--------------------------------------------------------------#

# Genesys Data (all genesys): standardized, unfiltered
WCFP_Genesys_data_all <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_Genesys_data_all_standardized_2026-03-03.csv")

## BGCI Data (combined original WCFP sp + new sp): standardized, unfiltered
WCFP_BGCI_data_all <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_BGCI_data_all_standardizedWFO_2026-02-23.csv')

## Cano et al Data (combined original WCFP sp + new sp): already standardized/filtered
WCFP_Cano_data <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Raw_data/LCs_food26.xlsx") %>%
  mutate(data_source = "Cano et al")

## GBIF data (combined original WCFP sp + new sp): standardized, unfiltered: 7,963,498 rows
WCFP_GBIF_data_all <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_GBIF_raw_data_ALL_standardized_2026-03-10.csv")

# WIEWS data (wcfp original sp + new sp): standardized, unfiltered: 3,236,589 rows
WCFP_WIEWS_data_all <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_WIEWS_data_all_standardized_2026-02-26.csv")





################################################################################
######## DATA PROCESSING #######################################################
################################################################################

#----------------------------------#
#--- Genesys filter for WCFP ------#
#-----------------------------------#

# Genesys: fill in taxa standardization that failed
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(
    Standardized_taxa = ifelse(
      Standardized_taxa == "no_match",
      word(taxa, 1, 2),
      Standardized_taxa ))

# Add genus_species_WFO column to Genesys data using Standardized_taxa
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(genus_species = extract_genus_species(taxa),
         genus_species_WFO = extract_genus_species(Standardized_taxa))

# Prepare exact match terms from WCFP plantlist (all plantlist genus_species and genus_species_WFO values in one column)
exact_terms <- WCFP_plantlist_stand %>%
  select(genus_species, genus_species_WFO) %>%
  pivot_longer(everything(), values_to = "term") %>%
  filter(!is.na(term)) %>%
  distinct(term)

# Filter rows where genus_species OR genus_species_WFO matches any plantlist term
WCFP_Genesys_data_filtered <- WCFP_Genesys_data_all  %>%
  filter(
    genus_species     %in% exact_terms$term |
      genus_species_WFO %in% exact_terms$term)

# Check the dropped rows
Genesys_dropped_rows <- anti_join(WCFP_Genesys_data_all , WCFP_Genesys_data_filtered,
                                  by = c("genus_species", "genus_species_WFO"))

# Save Genesys data: standardized + filtered: 3,618,693 rows
write.csv(WCFP_Genesys_data_filtered,'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized_and_filtered/WCFP_Genesys_data_filtered_2026-03-05.csv', row.names = FALSE)





#--------------------------#
#--- BGCI filter for WCFP -#
#--------------------------#

# BGCI: Standardize field names
WCFP_BGCI_data_all <- WCFP_BGCI_data_all %>%
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
    germplasm_explant = Germplasm..explant)

# Assign storage type (only for BGCI)
WCFP_BGCI_data_all <- WCFP_BGCI_data_all %>%
  mutate(storage_type = paste(
    ifelse(germplasm_plant == 1, "plant", NA),
    ifelse(germplasm_seed == 1, "seed", NA),
    ifelse(germplasm_pollen == 1, "pollen", NA),
    ifelse(germplasm_explant == 1, "explant", NA),
    sep = "; "
  )) %>%
  mutate(storage_type = gsub("(^NA; |; NA$|; NA; |NA)", "", storage_type)) %>%
  mutate(storage_type = ifelse(storage_type == "", NA, storage_type))

# Add genus_species column
WCFP_BGCI_data_all <- WCFP_BGCI_data_all %>%
  mutate(genus_species = extract_genus_species(taxa))

# Add genus_species_WFO column
WCFP_BGCI_data_all <- WCFP_BGCI_data_all %>%
  mutate(genus_species_WFO = extract_genus_species(Standardized_taxa))

# Prepare exact match terms from WCFP Plantlist (all plantlist genus_species and genus_species_WFO values in one column)
exact_terms <- WCFP_plantlist_stand %>%
  select(genus_species, genus_species_WFO) %>%
  pivot_longer(everything(), values_to = "term") %>%
  filter(!is.na(term)) %>%
  distinct(term)

# Filter rows where genus_species OR genus_species_WFO matches any plantlist term
WCFP_BGCI_data_filtered <- WCFP_BGCI_data_all  %>%
  filter(
    genus_species     %in% exact_terms$term |
      genus_species_WFO %in% exact_terms$term)

# Check the dropped rows, 5,217 rows
BGCI_dropped_rows <- anti_join(WCFP_BGCI_data_all, WCFP_BGCI_data_filtered,
                               by = c("genus_species", "genus_species_WFO"))

# Save BGCI: standardized + filtered for WCFP
# 626,307 rows
df_save_results <- apply(WCFP_BGCI_data_filtered, 2, as.character)
write.csv(df_save_results, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized_and_filtered/WCFP_BGCI_data_filtered_2026-03-09.csv', row.names = FALSE)




#----------------------------------#
#--- Cano et al. filter for WCFP -#
#---------------------------------#

# Cano: Standardize field names
WCFP_Cano_data <- WCFP_Cano_data %>%
  rename(
    item_status_type = ItemStatusType,
    taxon_name = TaxonName,
    item_acc_no_full = ItemAccNoFull,
    acc_no_full = AccNoFull,
    acc_year = AccYear,
    genus_species_notstand = GenusSpecies,
    provenance_code = ProvenanceCode,
    item_status_date = ItemStatusDate,
    taxon_name_simple = TaxonName_simple,
    cultivar_name = Cultivar_name,
    family = ALL_families,
    acc_numb = acc )

# Taxonomic name check: 
# prepare a new list of genus_species
WCFP_Cano_data <- WCFP_Cano_data %>%
  mutate(genus_species = extract_genus_species(taxon_name))

# Prepare pattern terms
pattern_terms <- c(WCFP_plantlist_stand$genus_species, WCFP_plantlist_stand$genus_species_WFO) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Create regex pattern
pattern <- str_c("(?i)", str_c(str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

# Filter all matching rows
WCFP_Cano_filtered <- WCFP_Cano_data %>%
  filter(str_detect(genus_species, regex(pattern, ignore_case = TRUE)))

# check the dropped rows, there are zero
Cano_dropped_rows <- anti_join(WCFP_Cano_data, WCFP_Cano_filtered, by = "genus_species")

# Save Cano data: (already standardized) + filtered
write.csv(WCFP_Cano_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized_and_filtered/WCFP_Cano_data_filtered_2026-02-23.csv', row.names = FALSE)






#----------------------------------#
#--- GBIF filter for WCFP -------- #
#--------------------------------- #

# Add field: standardized_genus
WCFP_GBIF_data_all <- WCFP_GBIF_data_all %>%
  mutate(standardized_genus = str_to_title(word(genus_species_WFO, 1)))

# Prepare exact match terms from WCFP plantlist (all plantlist genus_species and genus_species_WFO values in one column)
exact_terms <- WCFP_plantlist_stand %>%
  select(genus_species, genus_species_WFO) %>%
  pivot_longer(everything(), values_to = "term") %>%
  filter(!is.na(term)) %>%
  distinct(term)

# Filter rows where genus_species OR genus_species_WFO matches any plantlist term
WCFP_GBIF_data_filtered <- WCFP_GBIF_data_all %>%
  filter(
    genus_species     %in% exact_terms$term |
      genus_species_WFO %in% exact_terms$term )

# Check the dropped rows
# 1,143,518 rows dropped
GBIF_dropped_rows <- anti_join(WCFP_GBIF_data_stand_combined, WCFP_GBIF_data_filtered,
                               by = c("genus_species", "genus_species_WFO"))

# Save, NEW FILTERED GBIF
# 6,819,980 rows
write.csv(WCFP_GBIF_data_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized_and_filtered/WCFP_GBIF_data_filtered_2026-03-10.csv', row.names = FALSE)





########## ADD WCFP NAME MATCH FIELD ##########
# Create a lookup with all possible name combinations from WCFP_plantlist_stand
# Build lookup from genus_species and genus_species_WFO separately then combine
wcfp_lookup <- bind_rows(
  # genus_species lookup
  WCFP_plantlist_stand %>%
    select(name = genus_species, taxon_name_accepted) %>%
    filter(!is.na(name) & name != ""),
  
  # genus_species_WFO lookup
  WCFP_plantlist_stand %>%
    select(name = genus_species_WFO, taxon_name_accepted) %>%
    filter(!is.na(name) & name != "")
) %>%
  mutate(name = tolower(str_trim(name))) %>%  # lowercase + trim whitespace
  distinct()

# Match against both genus_species and genus_species_WFO and genus_species_original in WIEWS_allcrops
# Unmatched rows will simply be NA
WCFP_GBIF_data_filtered <- WCFP_GBIF_data_filtered %>%
  mutate(WCFP_name_match = coalesce(
    wcfp_lookup$taxon_name_accepted[match(tolower(str_trim(genus_species)), wcfp_lookup$name)],
    wcfp_lookup$taxon_name_accepted[match(tolower(str_trim(genus_species_WFO)), wcfp_lookup$name)]  ))

# View rows with no match, ZERO
na_rows <- WCFP_GBIF_data_filtered %>% filter(is.na(WCFP_name_match))

# SAVE 
# 6,819,980 rows
write.csv(WCFP_GBIF_data_filtered, "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_all_data_prepped_2026-03-10.csv", row.names = FALSE)



# FILTER GBIF=LIVING AND GBIF= ALL ELSE EXCEPT FOSSIL
# 6,819,980 rows
WCFP_GBIF_data_filtered <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_all_data_prepped_2026-03-10.csv")

# DROP data_source = GBIF
WCFP_GBIF_data_filtered <- WCFP_GBIF_data_filtered %>% select(-data_source)

# FILTER for basis of record = LIVING
# 118,198 rows
WCFP_GBIF_data_living <- WCFP_GBIF_data_filtered %>%
  filter(basis_of_record == "LIVING_SPECIMEN") %>%
  mutate(data_source = "GBIF_living")

# FILTER for observations: basis of record = all else (make sure fossils are not included)
# 6,701,782 rows
WCFP_GBIF_data_observations <- WCFP_GBIF_data_filtered %>%
  filter(basis_of_record != "LIVING_SPECIMEN") %>%
  mutate(data_source = "GBIF_observations")

# SAVE GBIF LIVING data: Standardized and filtered: 118,198 rows
write.csv(WCFP_GBIF_data_living, "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_living_2026-03-11.csv", row.names = FALSE)

# SAVE GBIF OBSERVATIONS: Standardized and filtered: 6,701,782 rows
write.csv(WCFP_GBIF_data_observations, "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_observations_2026-03-11.csv", row.names = FALSE)






#----------------------------------#
#--- WIEWS filter for WCFP -------- #
#--------------------------------- #

#DROP old genus_species field (genus+species function updated)
WCFP_WIEWS_data_all2 <- WCFP_WIEWS_data_all2 %>% select(-genus_species)

# Add genus_species using accepted taxa
WCFP_WIEWS_data_all2 <- WCFP_WIEWS_data_all2 %>%
  mutate(genus_species = extract_genus_species(taxa))

# Add genus_species_WFO using Standardized taxa
WCFP_WIEWS_data_all2 <- WCFP_WIEWS_data_all2 %>%
  mutate(genus_species_WFO = extract_genus_species(Standardized_taxa))

# Add genus_species using original TAXON
WCFP_WIEWS_data_all2 <- WCFP_WIEWS_data_all2 %>%
  mutate(genus_species_original = extract_genus_species(TAXON))

# Filter based on WCFP standardized list: Prepare pattern terms
pattern_terms <- c(WCFP_plantlist_stand$genus_species, WCFP_plantlist_stand$genus_species_WFO) %>%
  discard(is.na) %>%
  str_trim() %>%
  unique()

# Create regex pattern
pattern <- str_c("(?i)", str_c(str_replace_all(pattern_terms, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|"))

# Filter all matching rows from WCFP plantlist (genus_species OR genus_species_WFO or genus_species_original)
WCFP_WIEWS_filtered <- WCFP_WIEWS_data_all2 %>%
  filter(
    str_detect(genus_species, regex(pattern, ignore_case = TRUE)) |
      str_detect(genus_species_original, regex(pattern, ignore_case = TRUE)) |
      str_detect(genus_species_WFO, regex(pattern, ignore_case = TRUE)))

# check the dropped rows, there are 26
WIEWS_dropped_rows <- anti_join(WCFP_WIEWS_data_all2 , WCFP_WIEWS_filtered,,
                                by = c("genus_species", "genus_species_WFO", "genus_species_original"))

#SAVE WIEWS data: standardized and filtered for WCFP
write.csv(WCFP_WIEWS_filtered, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized_and_filtered/WCFP_WIEWS_data_filtered_2026-03-05.csv', row.names = FALSE)




### End script ####