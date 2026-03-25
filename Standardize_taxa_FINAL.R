### Project: Agrobiodiversity, GCCFP, WCFP Complementarity ###
### Data cleaning and filtering for new WCFP plantlist data
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

source("Functions/query_taxa_resolver.R") # Functions for taxa standardization
source("Functions/extract_best_result.R")
source("Functions/standardize_taxa.R")
source("Functions/extract_genus_species.R") # Function to normalize genus + species


#---------------------------------#
#--- WCFP Plant List Read-In -----#
#---------------------------------#

# WCFP plant list NEW, 26,632 species
WCFP_plantlist <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/WCFP_plantlist_new/WCFP_251103.xlsx")


#---------------------------#
#--- RAW Data Read-In -----#
#--------------------------#

## Genesys Data (all genesys)
WCFP_Genesys_data_all <- read.csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Raw_data/colin_dataset_from_Christelle_2025-03-25.csv", sep = ";") %>%
  mutate(data_source = "Genesys")

## BGCI Data
WCFP_BGCI_data_old <- list.files("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Raw_data/exports from PS", full.names = TRUE, pattern = "\\.csv$") %>% map_df(read.csv)
#BGCI Data (2026 data new species)
WCFP_BGCI_new_species <- read_csv("Data/Raw_data/BGCI_WCFP_plantlist_new_species.csv")
#combine new species BGCI data with old data export
WCFP_BGCI_data_all <- bind_rows(WCFP_BGCI_data_old, WCFP_BGCI_new_species) %>% mutate(data_source = "BGCI")

## GBIF data (combined GBIF data old + new sp): 7,965,039 rows
WCFP_GBIF_data_all <- read.csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/GBIF_data_all.csv")

# WIEWS data
WCFP_WIEWS_data_all <- read.csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Raw_data/WIEWS/WIEWS_WCFP.txt", 
                                header = TRUE,    # First row contains column names
                                sep = ",",        # Comma-separated
                                stringsAsFactors = FALSE)  # Keep text as characters



#------------------------------------------#
#--- WCFP Plant List Taxa Standardization -#
#------------------------------------------#

# WCFP Plant List: Taxa Standardization
WCFP_plantlist <- WCFP_plantlist %>% # combine accepted authors with taxon in a new field called taxa to standardize
  mutate(taxa = paste(taxon_name_accepted, taxon_authors_accepted, sep = " "))
WCFP_plantlist <- WCFP_plantlist %>%
  mutate(
    taxa = taxa %>%
      str_replace_all("\t", " ") %>%     # Replace tabs with spaces
      str_remove_all("\\+") %>%          # Remove plus signs
      str_squish()                       # Trim extra whitespace
  )

plantlist_taxa_list <- unique(trimws(na.omit(WCFP_plantlist$taxa)))

result_queries_WFO <- map(plantlist_taxa_list, ~ query_taxa_resolver(.x, c('196')))
res_WFO <- extract_best_result(result_queries_WFO)
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
# unlist
taxa_standardized_df_WFO <- data.frame(lapply(taxa_standardized_df_WFO, function(column) {
  if (is.list(column)) {
    return(unlist(column))
  } else {
    return(column)
  }
}))
#save WCFP taxa table
write.csv(taxa_standardized_df_WFO, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/WCFP_plantlist/Standardized/WCFP_plantlist_standardized_taxaTABLE_2026-02-23.csv', row.names = FALSE)

standardization_table_PlantList <- setNames(taxa_standardized_df_WFO$output_name_WFO, taxa_standardized_df_WFO$input_name)
WCFP_plantlist <- standardize_taxa(WCFP_plantlist, "taxa", standardization_table_PlantList)
WCFP_plantlist <- WCFP_plantlist %>%
  mutate(
    Standardized_taxa = ifelse(  # only keep taxa standardized to a differing name
      word(taxa, 1, 2) == word(Standardized_taxa, 1, 2),
      "",
      Standardized_taxa))
# ADD GENUS_SPECIES field
WCFP_plantlist <-  WCFP_plantlist %>%
  mutate(genus_species = extract_genus_species(taxon_name_accepted),
         genus_species_WFO = extract_genus_species(Standardized_taxa))
# Save WCFP standardized plantlist
WCFP_plantlist$Standardized_taxa <- unlist(WCFP_plantlist$Standardized_taxa) #unlist
write_xlsx(WCFP_plantlist, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/WCFP_plantlist/Standardized/WCFP_plantlist_standardized_2026-02-23.xlsx')





#----------------------------------#
#--- Genesys Taxa Standardization -#
#-----------------------------------#

# Genesys: Create taxa field
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(taxa = paste(GENUS, SPECIES, SPAUTHOR, SUBTAXA, SUBTAUTHOR, sep = " "))
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(
    taxa = taxa %>%
      str_replace_all("\t", " ") %>%     # Replace tabs with spaces
      str_remove_all("\\+") %>%          # Remove plus signs
      str_remove_all("\\?") %>%          # Remove question marks
      str_squish()                       # Trim and collapse whitespace
  )

# Genesys: Taxa Standardization
genesys_taxa_list <- unique(trimws(na.omit(WCFP_Genesys_data_all$taxa)))
result_queries_WFO <- map(genesys_taxa_list, ~ query_taxa_resolver(.x, c('196')))
res_WFO <- extract_best_result(result_queries_WFO)
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')

#save taxa table
taxa_standardized_df_WFO <- taxa_standardized_df_WFO %>%
  mutate(across(1:5, ~ sapply(., function(x) paste(unlist(x), collapse = "; "))))
write.csv(taxa_standardized_df_WFO, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_Genesys_standardized_taxa_TABLE_WFO_2026-03-03.csv', row.names = FALSE)

#### Add Standardized_taxa column to Genesys dataset
# read in Taxa Dictionary (if picking up from here)
standardization_table_Genesys <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_Genesys_standardized_taxa_TABLE_WFO_2026-03-03.csv")
# Structure standardization table for easy look up
standardization_table_Genesys <- as.data.frame(standardization_table_Genesys, stringsAsFactors = FALSE)
standardization_table_Genesys <- setNames(standardization_table_Genesys$output_name_WFO, standardization_table_Genesys$input_name)
# Make Standardized_taxa column
WCFP_Genesys_data_all$Standardized_taxa <- NA
# Trim whitespace in taxa field
WCFP_Genesys_data_all$taxa <- trimws(WCFP_Genesys_data_all$taxa)
# Add standardized taxa field to Genesys dataset
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(Standardized_taxa = standardization_table_Genesys[match(taxa, names(standardization_table_Genesys))])

# SAVE raw genesys file with standardized names, 2026_03_03
write.csv(WCFP_Genesys_data_all, "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_Genesys_data_all_standardized_2026-03-03.csv", row.names = FALSE)





#-------------------------------#
#--- BGCI Taxa Standardization -#
#-------------------------------#

# Create taxa lists of accepted taxa and synonyms
bgci_taxa_list <- unique(trimws(na.omit(WCFP_BGCI_data_all$Accepted.Name..in.PlantSearch.)))
bgci_syn_list <- unique(trimws(na.omit(WCFP_BGCI_data_all$Synonymous.Name..in.PlantSearch.)))

# Standardize accepted names
result_queries_WFO <- map(bgci_taxa_list, ~ query_taxa_resolver(.x, c('196')))
res_WFO <- extract_best_result(result_queries_WFO)
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
#save
df_save_results <- apply(taxa_standardized_df_WFO, 2, as.character)
write.csv(df_save_results, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_BGCI-PlantSearch_standardized_taxa_WFO_2025-02-23.csv', row.names = FALSE)
standardization_table_BGCI <- setNames(taxa_standardized_df_WFO$output_name_WFO, taxa_standardized_df_WFO$input_name)

# Standardize synonyms
result_queries_WFO <- map(bgci_syn_list, ~ query_taxa_resolver(.x, c('196')))
res_syn_WFO <- extract_best_result(result_queries_WFO)
taxa_syn_standardized_df_WFO <- as.data.frame(do.call(rbind, res_syn_WFO))
colnames(taxa_syn_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')
#save
df_save_results <- apply(taxa_syn_standardized_df_WFO, 2, as.character)
write.csv(df_save_results, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_BGCI-PlantSearch_standardized_syntaxa_WFO_2025-02-23.csv', row.names = FALSE)
syn_standardization_table_BGCI <- setNames(taxa_syn_standardized_df_WFO$output_name_WFO, taxa_syn_standardized_df_WFO$input_name)

# BGCI: Add standardized taxa/synonyms
WCFP_BGCI_data_all2 <- WCFP_BGCI_data_all %>%
  mutate(
    Standardized_taxa = standardization_table_BGCI[Accepted.Name..in.PlantSearch.],
    Standardized_syntaxa = syn_standardization_table_BGCI[Synonymous.Name..in.PlantSearch.]
  ) %>%
  mutate(Standardized_taxa = coalesce(Standardized_taxa, Standardized_syntaxa)) %>%
  select(-Standardized_syntaxa)

# BGCI: Drop rows where both accepted and synonym are NA
WCFP_BGCI_data_all3 <- WCFP_BGCI_data_all2 %>%
  filter(!( (is.na(Accepted.Name..in.PlantSearch.) | Accepted.Name..in.PlantSearch. == "") &
              (is.na(Synonymous.Name..in.PlantSearch.) | Synonymous.Name..in.PlantSearch. == "") ))

# Read standardization table(IF PICKING UP FROM HERE)
standardization_table_BGCI <- read_csv("Data/Processed_data/Standardized/WCFP_BGCI-PlantSearch_standardized_taxa_WFO_2026-02-23.csv")
# Structure standardization table for easy look up
standardization_table_BGCI <- as.data.frame(standardization_table_BGCI, stringsAsFactors = FALSE)
standardization_table_BGCI <- setNames(standardization_table_BGCI$output_name_WFO, standardization_table_BGCI$input_name)
# Make Standardized_taxa column
WCFP_BGCI_data_all$Standardized_taxa <- NA

# ASSIGN TAXA FIELD FOR MATCH
WCFP_BGCI_data_all <- WCFP_BGCI_data_all %>% rename(taxa = Accepted.Name..in.PlantSearch.)
# Trim whitespace in taxa field
WCFP_BGCI_data_all$taxa <- trimws(WCFP_BGCI_data_all$taxa)

# Add standardized taxa field to BGCI dataset
WCFP_BGCI_data_all <- WCFP_BGCI_data_all %>%
  mutate(Standardized_taxa = standardization_table_BGCI[match(taxa, names(standardization_table_BGCI))])

# read in standardized SYN names
standardization_table_BGCI_syn <- read_csv("Data/Processed_data/Standardized/WCFP_BGCI-PlantSearch_standardized_syntaxa_WFO_2026-02-23.csv")
# Structure standardization table for easy look up
standardization_table_BGCI_syn <- as.data.frame(standardization_table_BGCI_syn, stringsAsFactors = FALSE)
standardization_table_BGCI_syn <- setNames(standardization_table_BGCI_syn$output_name_WFO, standardization_table_BGCI_syn$input_name)
# Make Standardized_taxa column
WCFP_BGCI_data_all$Standardized_syn_taxa <- NA

# ASSIGN TAXA FIELD FOR MATCH
WCFP_BGCI_data_all <- WCFP_BGCI_data_all %>% rename(taxa2 = Synonymous.Name..in.PlantSearch.)
# Trim whitespace in taxa field
WCFP_BGCI_data_all$taxa2 <- trimws(WCFP_BGCI_data_all$taxa2)

# Add standardized SYN taxa field to BGCI dataset
WCFP_BGCI_data_all <- WCFP_BGCI_data_all %>%
  mutate(Standardized_syn_taxa = standardization_table_BGCI_syn[match(taxa2, names(standardization_table_BGCI_syn))])


# RENAME BACK FOR EASY REVIEW
WCFP_BGCI_data_all <- WCFP_BGCI_data_all %>% rename(Accepted.Name..in.PlantSearch. = taxa)
WCFP_BGCI_data_all <- WCFP_BGCI_data_all %>% rename(Synonymous.Name..in.PlantSearch. = taxa2)

# BGCI: Drop rows where both the original -accepted name and origina synonym are NA (no match in BGCI search)
# 7,739 dropped row (empty rows from BGCI)
WCFP_BGCI_data_all2 <- WCFP_BGCI_data_all %>%
  filter(!( (is.na(Accepted.Name..in.PlantSearch.) | Accepted.Name..in.PlantSearch. == "") &
              (is.na(Synonymous.Name..in.PlantSearch.) | Synonymous.Name..in.PlantSearch. == "") ))

# COALESCE Standardized_taxa into one field
WCFP_BGCI_data_all2 <- WCFP_BGCI_data_all2 %>%
  mutate(Standardized_taxa = coalesce(Standardized_taxa, Standardized_syn_taxa)) %>%
  select(-Standardized_syn_taxa)

# COALESCE taxa into one field
#first replace blanks with nas 
WCFP_BGCI_data_all2 <- WCFP_BGCI_data_all2 %>%
  mutate(Accepted.Name..in.PlantSearch. = na_if(trimws(Accepted.Name..in.PlantSearch.), "")) %>%
  mutate(Synonymous.Name..in.PlantSearch. = na_if(trimws(Synonymous.Name..in.PlantSearch.), ""))
WCFP_BGCI_data_all2 <- WCFP_BGCI_data_all2 %>%
  mutate(taxa = coalesce(Accepted.Name..in.PlantSearch., Synonymous.Name..in.PlantSearch.))

#save BGCI data: standardized
write.csv(WCFP_BGCI_data_all2, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_BGCI_data_all_standardizedWFO_2026-02-23.csv', row.names = FALSE)




#-------------------------------#
#--- GBIF Taxa Standardization -#
#-------------------------------#

# Create duplicate of taxa field
WCFP_GBIF_data_all$taxa_original <- WCFP_GBIF_data_all$taxa

# GBIF: Clean taxa field
WCFP_GBIF_data_all <- WCFP_GBIF_data_all %>%
  mutate(
    taxa = taxa %>%
      str_replace_all("\t", " ") %>%     # Replace tabs with spaces
      str_remove_all("\\+") %>%          # Remove plus signs
      str_remove_all("\\?") %>%          # Remove question marks
      str_squish()                       # Trim and collapse whitespace
  )

# GBIF: Taxa Standardization
gbif_taxa_list <- unique(trimws(na.omit(WCFP_GBIF_data_all$taxa)))
result_queries_WFO <- map(gbif_taxa_list, ~ query_taxa_resolver(.x, c('196')))

res_WFO <- extract_best_result(result_queries_WFO)
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')

taxa_standardized_df_WFO2 <- taxa_standardized_df_WFO %>%
  mutate(across(1:5, ~ sapply(., function(x) paste(unlist(x), collapse = "; "))))

#save GBIF taxa table
write.csv(taxa_standardized_df_WFO2, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_GBIF_standardized_taxaTABLE_WFO2026-02-23.csv', row.names = FALSE)


# Add Standardized taxa to GBIF
gbif_taxa_standardized_df_WFO <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_GBIF_standardized_taxaTABLE_WFO2026-02-23.csv')
# Structure standardization table for easy look up
gbif_taxa_standardized_df_WFO <- as.data.frame(gbif_taxa_standardized_df_WFO, stringsAsFactors = FALSE)
gbif_taxa_standardized_df_WFO <- setNames(gbif_taxa_standardized_df_WFO$output_name_WFO, gbif_taxa_standardized_df_WFO$input_name)
# Make Standardized_taxa column
WCFP_GBIF_data_not_stand$Standardized_taxa <- NA
# Add GBIF standardized taxa field to GBIF dataset
WCFP_GBIF_data_all2 <- WCFP_GBIF_data_all %>%
  mutate(Standardized_taxa = gbif_taxa_standardized_df_WFO[match(taxa, names(gbif_taxa_standardized_df_WFO))])

# DROP 2,141 ROWS WHERE TAXA IS BLANK
WCFP_GBIF_data_all2 <- WCFP_GBIF_data_all2 %>% filter(!is.na(taxa) & taxa != "")

# Check no match rows: 372,002
no_match_rows <- WCFP_GBIF_data_all2 %>% filter(Standardized_taxa == "no_match")

# Replace no_match with na
WCFP_GBIF_data_all2 <- WCFP_GBIF_data_all2 %>%
  mutate(Standardized_taxa = na_if(Standardized_taxa, "no_match"))

# Fill in standardized_taxa (if blank) with taxa
WCFP_GBIF_data_all2 <- WCFP_GBIF_data_all2 %>%
  mutate(Standardized_taxa = ifelse(is.na(Standardized_taxa) | Standardized_taxa == "", taxa, Standardized_taxa))

# rename taxa to taxa_copy
WCFP_GBIF_data_all2 <- WCFP_GBIF_data_all2 %>% rename(taxa_copy = taxa)

# rename taxa_original back to taxa
WCFP_GBIF_data_all2 <- WCFP_GBIF_data_all2 %>% rename(taxa = taxa_original)

# DROP TAXA COPY
WCFP_GBIF_data_all2 <- WCFP_GBIF_data_all2 %>% select(-taxa_copy)

#reorder fields
WCFP_GBIF_data_all2 <- WCFP_GBIF_data_all2 %>%
  select(taxa, basis_of_record, latitude, longitude, country_code, country,
         state_province, locality, inst_code, collection_code, data_source, Standardized_taxa)

# Add genus_species using taxa
WCFP_GBIF_data_all2 <- WCFP_GBIF_data_all2 %>%
  mutate(genus_species = extract_genus_species(taxa))

# Add genus_species_WFO using Standardized taxa
WCFP_GBIF_data_all2 <- WCFP_GBIF_data_all2 %>%
  mutate(genus_species_WFO = extract_genus_species(Standardized_taxa))

# save GBIF data: all STANDARDIZED: 7,963,498 rows
write.csv(WCFP_GBIF_data_all2, "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_GBIF_raw_data_ALL_standardized_2026-03-10.csv", row.names = FALSE)





#--------------------------------#
#--- WIEWS Taxa Standardization -#
#--------------------------------#

# WIEWS: Create taxa field combining ACC_GENUS and ACC_SPECIES
# taxa = ACCEPTED TAXA
WCFP_WIEWS_data_all <- WCFP_WIEWS_data_all %>%
  mutate(taxa = paste(ACC_GENUS, ACC_SPECIES, sep = " "))

WCFP_WIEWS_data_all <- WCFP_WIEWS_data_all %>%
  mutate(
    taxa = taxa %>%
      str_replace_all("\t", " ") %>%     # Replace tabs with spaces
      str_remove_all("\\+") %>%          # Remove plus signs
      str_remove_all("\\?") %>%          # Remove question marks
      str_squish()                       # Trim and collapse whitespace
  )

# WIEWS: Taxa Standardization
wiews_taxa_list <- unique(trimws(na.omit(WCFP_WIEWS_data_all$taxa)))
result_queries_WFO <- map(wiews_taxa_list, ~ query_taxa_resolver(.x, c('196')))
res_WFO <- extract_best_result(result_queries_WFO)
taxa_standardized_df_WFO <- as.data.frame(do.call(rbind, res_WFO))
colnames(taxa_standardized_df_WFO) <- c('input_name', 'matched_name_WFO', 'match_type_WFO', 'status_WFO', 'output_name_WFO')

taxa_standardized_df_WFO <- taxa_standardized_df_WFO %>%
  mutate(across(1:5, ~ sapply(., function(x) paste(unlist(x), collapse = "; "))))

#save taxa table
write.csv(taxa_standardized_df_WFO, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_WIEWS_standardized_taxaTABLE_WFO2026-02-26.csv', row.names = FALSE)


#### Add Standardized_taxa column to WIEWS dataset
# read in Taxa Dictionary (if picking up from here)
standardization_table_WIEWS <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_WIEWS_standardized_taxaTABLE_WFO2026-02-26.csv")
# Structure standardization table for easy look up
standardization_table_WIEWS <- as.data.frame(standardization_table_WIEWS, stringsAsFactors = FALSE)
standardization_table_WIEWS <- setNames(standardization_table_WIEWS$output_name_WFO, standardization_table_WIEWS$input_name)
# Make Standardized_taxa column
WCFP_WIEWS_data_all$Standardized_taxa <- NA
# Trim whitespace in taxa field
WCFP_WIEWS_data_all$taxa <- trimws(WCFP_WIEWS_data_all$taxa)
# Add standardized taxa field to WIEWS dataset
WCFP_WIEWS_data_all2 <- WCFP_WIEWS_data_all %>%
  mutate(Standardized_taxa = standardization_table_WIEWS[match(taxa, names(standardization_table_WIEWS))])

# Add genus_species_WFO column to WIEWS data using Standardized_taxa
WCFP_WIEWS_data_all2 <- WCFP_WIEWS_data_all2 %>%
  mutate(genus_species_WFO = extract_genus_species(Standardized_taxa))

# save the raw WIEWS file with standardized names
write.csv(WCFP_WIEWS_data_all2, "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized/WCFP_WIEWS_data_all_standardized_2026-02-26.csv", row.names = FALSE)




### End script ###