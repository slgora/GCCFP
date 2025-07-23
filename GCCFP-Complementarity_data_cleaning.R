### Project: Agrobiodiversity, GCCFP ###
### Data exploration
### Script by Sarah Gora
### Date created: 2025_03_26
### Updated: 2025_07_22

library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(httr)
library(jsonlite)
library(stringr)

#----------------------#
#--- Helper Functions -#
#----------------------#

extract_genus_species <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("(?i)\\s*[×x]\\s*", " ") %>%
    str_squish() %>%
    str_extract("^\\S+\\s+\\S+")
}

standardize_taxa <- function(df, taxa_col, standardization_table) {
  df %>%
    mutate(
      !!taxa_col := trimws(.data[[taxa_col]]),
      Standardized_taxa = standardization_table[match(.data[[taxa_col]], names(standardization_table))]
    )
}

#----------------------#
#--- Data Read-In -----#
#----------------------#

# Plant List
WCFP_plantlist <- read_excel("C:/Users/sarah/Desktop/Agrobiodiversity/GCCFP/Plants_list/WCFP_simplified_240921.xlsx")

# Genesys Data
WCFP_Genesys_data_all <- read.csv("Data/colin_dataset_from_Christelle_2025-03-25.csv", sep = ";") %>%
  mutate(data_source = "Genesys")

# BGCI Data
WCFP_BGCI_data <- list.files("Data/exports from PS", full.names = TRUE, pattern = "\\.csv$") %>%
  map_df(read.csv) %>%
  mutate(data_source = "BGCI")

#----------------------#
#--- WCFP Plant List Taxa Standardization -#
#----------------------#
source("Functions/query_taxa_resolver.R")
source("Functions/extract_best_result.R")

# WCFP Plant List: Taxa Standardization

# combine accepted authors with taxon in a new field called taxa to standardize
WCFP_plantlist <- WCFP_plantlist %>%
  mutate(taxa = paste(taxon_name_accepted, taxon_authors_accepted, sep = " "))
WCFP_plantlist <- WCFP_plantlist %>%
  mutate(
    taxa = taxa %>%
      str_replace_all("\t", " ") %>%     # Replace tabs with spaces
      str_remove_all("\\+") %>%          # Remove plus signs
      str_squish()                       # Trim extra whitespace )

plantlist_taxa_list <- unique(trimws(na.omit(WCFP_plantlist$taxa)))

result_queries_WFO <- map(plantlist_taxa_list, ~ query_taxa_resolver(.x, c('196')))
res_WFO <- extract_best_result(result_queries_WFO)
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
write.csv(taxa_standardized_df_WFO, 'Outputs/WCFP_plantlist_standardized_taxaWFO_2025-07-23.csv', row.names = FALSE)


standardization_table_PlantList <- setNames(taxa_standardized_df_WFO$output_name_WFO, taxa_standardized_df_WFO$input_name)
WCFP_plantlist <- standardize_taxa(WCFP_plantlist, "taxa", standardization_table_PlantList)
WCFP_plantlist <- WCFP_plantlist %>%
  mutate(
    Standardized_taxa = ifelse(  # only keep taxa standardized to a differing name
      word(taxa, 1, 2) == word(Standardized_taxa, 1, 2),
      "",
      Standardized_taxa
    )
  )
# reject taxonomic standardization of taxa to just the Genus
names_reject <- c(
  "Aerva Forssk.",
  "Arthroceras Piirainen & G.Kadereit",
  "Dysphania R.Br.",
  "Oxybasis Kar. & Kir.",
  "Dioscorea Plum. ex L.",
  "Dovyalis E.Mey. ex Arn.",
  "Chenopodium L.",
  "Rumex L.",
  "Harpephyllum Bernh. ex Krauss",
  "Mimusops L.",
  "Koenigia L.",
  "Quercus subg. Quercus",
  "Talinum Adans.",
  "Grewia L.",
  "Jubaeopsis Becc.",
  "Rauvolfia L.",
  "Encephalartos Lehm.",
  "Cordia L.",
  "Monanthotaxis Baill.",
  "Uvaria L.",
  "Pimpinella L.",
  "Senegalia Raf.",
  "Ipomoea L.",
  "Bulbine Wolf",
  "Sideritis L.",
  "Erythrina L.",
  "Buchanania Spreng.",
  "Taxus L.",
  "Amomum L.",
  "Inga Mill.",
  "Holmbergia Hicken",
  "Aralia L.",
  "Asparagus L.",
  "Thottea Rottb.",
  "Ulva Haller",
  "Callicarpa L.",
  "Jatropha L.",
  "Bryopsis Reiche",
  "Phenax Wedd.",
  "Moraea Mill.",
  "Saurauia Willd.",
  "Euphorbia L.",
  "Taraxacum F.H.Wigg.",
  "Ximenia Plum. ex L.")
WCFP_plantlist <- WCFP_plantlist %>%
  mutate(
    Standardized_taxa = ifelse(Standardized_taxa %in% names_reject, "", Standardized_taxa))
write.csv(WCFP_plantlist, 'Outputs/WCFP_plantlist_standardized_2025-07-23.csv', row.names = FALSE)



#----------------------#
#--- Genesys Taxa Standardization -#
#----------------------#

# Genesys: Create taxa field
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(taxa = paste(GENUS, SPECIES, SUBTAXA, sep = " "))

# Genesys: Taxa Standardization
genesys_taxa_list <- unique(trimws(na.omit(WCFP_Genesys_data_all$taxa)))
result_queries_WFO <- map(genesys_taxa_list, ~ query_taxa_resolver(.x, c('196')))
res_WFO <- extract_best_result(result_queries_WFO)
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
write.csv(taxa_standardized_df_WFO, 'Data/Processing/Run_2025_07_18/WCFP_Genesys_standardized_taxa_WFO.csv', row.names = FALSE)

standardization_table_Genesys <- setNames(taxa_standardized_df_WFO$output_name_WFO, taxa_standardized_df_WFO$input_name)
WCFP_Genesys_data_all <- standardize_taxa(WCFP_Genesys_data_all, "taxa", standardization_table_Genesys)
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(Match_Found = !is.na(Standardized_taxa))
unmatched_taxa <- setdiff(unique(WCFP_Genesys_data_all$taxa), names(standardization_table_Genesys))
write.csv(unmatched_taxa, 'Outputs/gen_unmatched_taxa.csv', row.names = FALSE)

# Genesys: Add corrected taxa manually
gen_taxa_corrected <- read_excel("Data/gen_unmatched_taxa_corrected.xlsx")
WCFP_Genesys_data_all2 <- WCFP_Genesys_data_all %>%
  left_join(gen_taxa_corrected %>% select(taxa, Standardized_taxa), by = "taxa") %>%
  mutate(Standardized_taxa = coalesce(Standardized_taxa.x, Standardized_taxa.y)) %>%
  select(-Standardized_taxa.x, -Standardized_taxa.y)

WCFP_Genesys_data_all2 <- WCFP_Genesys_data_all2 %>%
  mutate(Standardized_taxa = ifelse(
    taxa == "Avena strigosa var. alba\tc. marquand",
    "Avena strigosa Schreb",
    Standardized_taxa
  ))

# Genesys: Standardize field names
WCFP_Genesys_data_all2 <- WCFP_Genesys_data_all2 %>%
  rename(
    uuid = UUID, historic = HISTORIC, inst_code = INSTCODE, acce_numb = ACCENUMB, doi = DOI,
    genus = GENUS, species = SPECIES, sp_author = SPAUTHOR, sub_taxa = SUBTAXA, sub_t_author = SUBTAUTHOR,
    samp_stat = SAMPSTAT, storage = STORAGE, orig_cty = ORIGCTY, latitude = DECLATITUDE, longitude = DECLONGITUDE,
    taxon_name_submitted = taxa, taxon_name_standardized_WFO = Standardized_taxa
  )

# Genesys: Filter for plant list crops
WCFP_plantlist <- WCFP_plantlist %>%
  mutate(genus_species = extract_genus_species(taxon_name_accepted))
pattern <- str_c(str_replace_all(WCFP_plantlist$taxon_name_accepted, "([\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\.\\|\\\\])", "\\\\\\1"), collapse = "|")
WCFP_Genesys_data_filtered <- WCFP_Genesys_data_all2 %>%
  filter(str_detect(taxon_name_standardized_WFO, pattern))

#----------------------#
#--- BGCI Taxa Standardization -#
#----------------------#
bgci_taxa_list <- unique(trimws(na.omit(WCFP_BGCI_data$Accepted.Name..in.PlantSearch.)))
bgci_syn_list <- unique(trimws(na.omit(WCFP_BGCI_data$Synonymous.Name..in.PlantSearch.)))

# Standardize accepted names
result_queries_WFO <- map(bgci_taxa_list, ~ query_taxa_resolver(.x, c('196')))
res_WFO <- extract_best_result(result_queries_WFO)
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
write.csv(taxa_standardized_df_WFO, 'Data/WCFP_BGCI-PlantSearch_standardized_taxa_WFO_06_03_25.csv', row.names = FALSE)
standardization_table_BGCI <- setNames(taxa_standardized_df_WFO$output_name_WFO, taxa_standardized_df_WFO$input_name)

# Standardize synonyms
result_queries_WFO <- map(bgci_syn_list, ~ query_taxa_resolver(.x, c('196')))
res_syn_WFO <- extract_best_result(result_queries_WFO)
taxa_syn_standardized_df_WFO <- as.data.frame(do.call(rbind, res_syn_WFO))
colnames(taxa_syn_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
write.csv(taxa_syn_standardized_df_WFO, 'Data/WCFP_BGCI-PlantSearch_standardized_syntaxa_WFO_06_25_25.csv', row.names = FALSE)
syn_standardization_table_BGCI <- setNames(taxa_syn_standardized_df_WFO$output_name_WFO, taxa_syn_standardized_df_WFO$input_name)

# BGCI: Add standardized taxa/synonyms
WCFP_BGCI_data2 <- WCFP_BGCI_data %>%
  mutate(
    Standardized_taxa = standardization_table_BGCI[Accepted.Name..in.PlantSearch.],
    Standardized_syntaxa = syn_standardization_table_BGCI[Synonymous.Name..in.PlantSearch.]
  ) %>%
  mutate(Standardized_taxa = coalesce(Standardized_taxa, Standardized_syntaxa)) %>%
  select(-Standardized_syntaxa)

# BGCI: Drop rows where both accepted and synonym are NA
WCFP_BGCI_data3 <- WCFP_BGCI_data2 %>%
  filter(!(is.na(Accepted.Name..in.PlantSearch.) & is.na(Synonymous.Name..in.PlantSearch.)))

# BGCI: Manual corrections
WCFP_BGCI_data3 <- WCFP_BGCI_data3 %>%
  mutate(Standardized_taxa = ifelse(Standardized_taxa == "× Butyagrus Vorster", "× Butyagrus nabonnandii", Standardized_taxa))

# BGCI: Standardize field names
WCFP_BGCI_data3 <- WCFP_BGCI_data3 %>%
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
    taxon_name_standardized_WFO = Standardized_taxa
  )

# BGCI: Normalize and match genus/species
WCFP_BGCI_data3_cleaned <- WCFP_BGCI_data3 %>%
  mutate(genus_species = extract_genus_species(taxon_name_standardized_WFO)) %>%
  mutate(Match_Found = genus_species %in% WCFP_plantlist$genus_species)

WCFP_BGCI_dropped_rows <- WCFP_BGCI_data3_cleaned %>%
  filter(!Match_Found)
write.csv(WCFP_BGCI_dropped_rows, 'Data/Processing/Run_2025_07_21/WCFP_BGCI_dropped_rows.csv', row.names = FALSE)

#----------------------#
#--- Institutions wrangling ---#
#----------------------#
WIEWS_instIDs <- read_excel("C:/Users/sarah/Desktop/GCCS-Metrics/Data/FAOWIEWS_data/WIEWS_instIDs.xlsx")
WIEWS_instIDs_dedup <- WIEWS_instIDs %>% group_by(ORGNAME_L) %>% slice(1) %>% ungroup()

bgci_df <- WCFP_BGCI_data3_cleaned %>%
  left_join(WIEWS_instIDs_dedup, by = c("ex_situ_site_name" = "ORGNAME_L")) %>%
  select(-ID, -ORGACRO_L, -VALID_ID, -DELETED) %>%
  rename(inst_code = WIEWS_INSTCODE)

genesys_df <- WCFP_Genesys_data_filtered %>%
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
internationalgenebanks_list <- read_excel("Data/internationalgenebanks_list.xlsx")
international_codes <- internationalgenebanks_list$instCode
genesys_df <- genesys_df %>%
  mutate(inst_status = ifelse(inst_code %in% international_codes, "International", "National"))

#----------------------#
#--- Data Exploration -#
#----------------------#
# Unique taxa counts
unique_taxa_count_bgci <- length(unique(bgci_df$taxon_name_accepted_PlantSearch[!is.na(bgci_df$taxon_name_accepted_PlantSearch)]))
unique_taxa_count_genesys <- length(unique(genesys_df$taxon_name_submitted[!is.na(genesys_df$taxon_name_submitted)]))

# Top institutions by accession count
gen_summary_by_instcode <- genesys_df %>%
  count(inst_code, name = "accessions_count") %>%
  arrange(desc(accessions_count)) %>%
  left_join(WIEWS_instIDs, by = c("inst_code" = "WIEWS_INSTCODE")) %>%
  select(-ID, -ORGACRO_L, -VALID_ID, -DELETED) %>%
  rename(org_name = ORGNAME_L)

bgci_summary_by_site <- bgci_df %>%
  count(ex_situ_site_name, name = "accessions_count") %>%
  arrange(desc(accessions_count))

#----------------------#
#--- TODO: Annotate Plant List ---#
#----------------------#
# Major/minor crops, cultivated/wild, food group annotation
# WCFP_plantlist <- read_excel("C:/Users/sarah/Desktop/Agrobiodiversity/GCCFP/Plants_list/WCFP_simplified_240921.xlsx")
# -- annotation steps to be determined --
