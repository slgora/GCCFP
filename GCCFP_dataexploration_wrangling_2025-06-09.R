### Project: Agrobiodiversity ###
### Data exploration
### Agrobiodiversity_dataexploration.R
### by Sarah Gora
### Date created: 2025_03_26

library(readr)
library(readxl)
library(dplyr)
library(purrr)

library(tidyr)
library(httr)
library(jsonlite)


################# Genesys Data Read In #########################################
# read in Genesys data export (4.8 million accessions, all of genesys accessions)
WCFP_Genesys_data_all <- read.csv("C:/Users/sarah/Desktop/Agrobiodiversity/GCCFP/Comparative_data/Genesys/colin_dataset_from_Christelle_2025-03-25.csv", sep = ";")
# combine GENUS and SPECIES column into a new column "taxa"
WCFP_Genesys_data_all <- WCFP_Genesys_data_all %>%
  mutate(taxa = paste(GENUS, SPECIES, sep = " "))
# annotate data source as genebanks
WCFP_Genesys_data_all <- cbind(WCFP_Genesys_data_all, data_source = "Genebank") # Add field: data_source


################## BGCI Plant Search Data Read In #############################
# read in BGCI Plant Search export data and combine into one file
file_paths <- list.files("C:/Users/sarah/Desktop/Agrobiodiversity/GCCFP/Comparative_data/BGCI_PlantSearch/WCFP/exports from PS", full.names = TRUE, pattern = "\\.csv$")
WCFP_BGCIPlantSearch_data <- file_paths %>%
  map_df(read.csv)
#annotate data source as Botanic gardens
WCFP_BGCIPlantSearch_data <- cbind(WCFP_BGCIPlantSearch_data, data_source = "Botanic garden") # Add field: data_source







############## Align Genesys and BGCI PS taxon to WFO ###########################

# Load Data & Functions
df <- WCFP_Genesys_data_all

# import function query_taxa_resolver
# function to query API of https://verifier.globalnames.org
query_taxa_resolver <- function(taxa, sources = c('196')) {
  if (!is.character(taxa)) return("Invalid input")
  
  taxa_format <- gsub(" ", "+", taxa)
  URL <- paste0('https://verifier.globalnames.org/api/v1/verifications/', taxa_format,
                '?data_sources=', paste(sources, collapse = "|"),
                '&all_matches=false&capitalize=true&species_group=false&fuzzy_uninomial=false&stats=false&main_taxon_threshold=0.8')
  
  print(URL)  # Debugging step - make sure it's inside the function!
  
  tryCatch({
    r <- GET(URL)
    if (r$status_code != 200) {
      stop("API request failed with status: ", r$status_code)
    }
    result <- content(r, "text", encoding = "UTF-8")
    return(fromJSON(result))
  }, error = function(e){
    print(paste("Error:", e$message))
    return(list(error="API request failed"))
  })
}
       
# import function extract_best_result
       # function to extract the best results from the query search 
       extract_best_result <- function(list_res){
         final <- list()
         for (i in list_res){
           # added to handle the case one of the results of the query is NULL
           if (is.null(i)) {
             final <- append(final, list(c('null', 'no_match', 'no_match', 'no_match', 'no_match')))
           } else if (!("names" %in% names(i))) {
             final <- append(final, list(c('null', 'no_match', 'no_match', 'no_match', 'no_match')))
           } else {
             match_type <- i["names"][[1]]["matchType"]
             if (match_type != "NoMatch"){
               input_name <-  i["names"][[1]]$name
               matched_name <-i["names"][[1]]$bestResult$matchedName
               output_name <- i["names"][[1]]$bestResult$currentName
               status <-  i["names"][[1]]$bestResult$taxonomicStatus
               final <- append(final, list(c(input_name, matched_name, match_type, status, output_name)))
             } else {
               final <- append(final, list(c( i["names"][[1]]$name, 'no_match', 'no_match', 'no_match', 'no_match')))
             }
           }}
         return(final)
       }
       

# Prepare Taxa List
taxa_list <- unique(trimws(na.omit(df$taxa)))

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

# Add Data Source Column
taxa_standardized_df_WFO <- cbind(taxa_standardized_df_WFO, data_source = "WFO")

# Save Results
df_save_results <- apply(taxa_standardized_df_WFO, 2, as.character)
write.csv(df_save_results, 'WCFP_Genesys_standardized_taxa_WFO_06_03_25.csv', row.names = FALSE)



#### add standardized_taxa column
# read in 
WCFP_Genesys_standardized_taxa <- read_csv("WCFP_Genesys_standardized_taxa_WFO_06_03_25.csv")

# standardization table is correctly structured
standardization_table_Genesys <- setNames(WCFP_Genesys_standardized_taxa$output_name_WFO, WCFP_Genesys_standardized_taxa$input_name)
# WCFP_Genesys_data_all has the Standardized_taxa column initialized
WCFP_Genesys_data_all$Standardized_taxa <- NA  
# Add standardized names from WFO to WCFP_Genesys_data_all
WCFP_Genesys_data_all2 <- WCFP_Genesys_data_all%>%
  mutate(Standardized_taxa = ifelse(!is.na(taxa) & taxa %in% names(standardization_table_Genesys), 
                                    standardization_table_Genesys[taxa], 
                                    Standardized_taxa))

# Save updated results
write.csv(WCFP_Genesys_data_all2, 'WCFP_Genesys_standardized_taxa_WFO_df_06_06_25.csv', row.names = FALSE)


# Standardize Genesys field names
WCFP_Genesys_data_all3 <- read_csv("WCFP_Genesys_standardized_taxa_WFO_df_06_06_25.csv")
# Standardize Genesys column names
WCFP_Genesys_data_all3 <- WCFP_Genesys_data_all3 %>%
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

## make new field from taxon_name_standardized_WFO
# long processing time so did this for ind dfs
# First, Clean parentheses in taxon names
WCFP_Genesys_data_all3 <- WCFP_Genesys_data_all3 %>%
  mutate(
    taxon_name_standardized_WFO = str_replace_all(taxon_name_standardized_WFO, "[()]", " ") # Replace parentheses with spaces
  )

# Extract the first two words (Genus & Species) and assign remaining text to a new column
WCFP_Genesys_data_all3 <- WCFP_Genesys_data_all3 %>%
  mutate(
    # Keep the full taxon name for reference
    taxon_and_authors_standardized_WFO = taxon_name_standardized_WFO,
    
    # Extract the first two words (Genus & Species)
    taxon_name_standardized_WFO = word(taxon_and_authors_standardized_WFO, 1, 2),
    
    # Extract everything after the first two words as authors
    taxon_authors_standardized_WFO = str_trim(str_replace(taxon_and_authors_standardized_WFO, paste0("^", taxon_name_standardized_WFO, "\\s*"), ""))
  )


## Filter for our crops/list
# read in plant list 
WCFP_plantlist <- read_excel("C:/Users/sarah/Desktop/Agrobiodiversity/GCCFP/Plants_list/WCFP_simplified_240921.xlsx")
# filter for WCFP crops 
# Keep rows in WCRP_Genesys_data_all3 where the taxa name (taxon_name_accepted) 
# in WCFP_plantlist matches the standardized taxa names (taxon_name_standardized_WFO) in WCRP_Genesys_data_all3
WCFP_Genesys_data_filtered <- WCFP_Genesys_data_all3 %>%
  filter(taxon_name_standardized_WFO %in% WCFP_plantlist$taxon_name_accepted)


# view the dropped rows:
# many rows part of the plant list so need to refine the taxa filtering step
dropped_rows <- anti_join(WCFP_Genesys_data_all3, WCFP_plantlist, 
                          by = c("taxon_name_standardized_WFO" = "taxon_name_accepted"))







############## Align BGCI Plant Search taxon to WFO ###########################

# Load Data & Functions
df <- WCFP_BGCIPlantSearch_data 

# import function query_taxa_resolver
# function to query API of https://verifier.globalnames.org
query_taxa_resolver <- function(taxa, sources = c('196')) {
  if (!is.character(taxa)) return("Invalid input")
  
  taxa_format <- gsub(" ", "+", taxa)
  URL <- paste0('https://verifier.globalnames.org/api/v1/verifications/', taxa_format,
                '?data_sources=', paste(sources, collapse = "|"),
                '&all_matches=false&capitalize=true&species_group=false&fuzzy_uninomial=false&stats=false&main_taxon_threshold=0.8')
  
  print(URL)  # Debugging step - make sure it's inside the function!
  
  tryCatch({
    r <- GET(URL)
    if (r$status_code != 200) {
      stop("API request failed with status: ", r$status_code)
    }
    result <- content(r, "text", encoding = "UTF-8")
    return(fromJSON(result))
  }, error = function(e){
    print(paste("Error:", e$message))
    return(list(error="API request failed"))
  })
}

# import function extract_best_result
# function to extract the best results from the query search 
extract_best_result <- function(list_res){
  final <- list()
  for (i in list_res){
    # added to handle the case one of the results of the query is NULL
    if (is.null(i)) {
      final <- append(final, list(c('null', 'no_match', 'no_match', 'no_match', 'no_match')))
    } else if (!("names" %in% names(i))) {
      final <- append(final, list(c('null', 'no_match', 'no_match', 'no_match', 'no_match')))
    } else {
      match_type <- i["names"][[1]]["matchType"]
      if (match_type != "NoMatch"){
        input_name <-  i["names"][[1]]$name
        matched_name <-i["names"][[1]]$bestResult$matchedName
        output_name <- i["names"][[1]]$bestResult$currentName
        status <-  i["names"][[1]]$bestResult$taxonomicStatus
        final <- append(final, list(c(input_name, matched_name, match_type, status, output_name)))
      } else {
        final <- append(final, list(c( i["names"][[1]]$name, 'no_match', 'no_match', 'no_match', 'no_match')))
      }
    }}
  return(final)
}


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

# Add Data Source Column
taxa_standardized_df_WFO <- cbind(taxa_standardized_df_WFO, data_source = "WFO")

# Save Results
df_save_results <- apply(taxa_standardized_df_WFO, 2, as.character)
write.csv(df_save_results, 'WCFP_BGCI-PlantSearch_standardized_taxa_WFO_06_03_25.csv', row.names = FALSE)


#### add standardized_taxa column
# read in 
WCFP_BGCI_PlantSearch_standardized_taxa <- read_csv("WCFP_BGCI-PlantSearch_standardized_taxa_WFO_06_03_25.csv")
# standardization table is correctly structured
standardization_table_BGCI <- setNames(WCFP_BGCI_PlantSearch_standardized_taxa$output_name_WFO, WCFP_BGCI_PlantSearch_standardized_taxa$input_name)
# WCFP_Genesys_data_all has the Standardized_taxa column initialized
WCFP_BGCIPlantSearch_data$Standardized_taxa <- NA  
# Add standardized names from WFO to WCFP_BGCIPlantSearch_data
WCFP_BGCIPlantSearch_data2 <- WCFP_BGCIPlantSearch_data%>%
  mutate(Standardized_taxa = ifelse(!is.na(Accepted.Name..in.PlantSearch.) & Accepted.Name..in.PlantSearch. %in% names(standardization_table_BGCI), 
                                    standardization_table_BGCI[Accepted.Name..in.PlantSearch.], 
                                    Standardized_taxa))
# save
write.csv(WCFP_BGCIPlantSearch_data2, 'WCFP_BGCI_PlantSearch_standardized_taxa_WFO_df_06_06_25.csv', row.names = FALSE)





# Standardize BGCI field names
# read in 
WCFP_BGCI_data_all3 <- read_csv("WCFP_BGCI_PlantSearch_standardized_taxa_WFO_df_06_06_25.csv")
# Standardize BGCI column names
WCFP_BGCI_data_all3 <- WCFP_BGCI_data_all3 %>%
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

## make new field from taxon_name_standardized_WFO
# long processing time so did this for ind dfs
# Extract the first two words (Genus & Species) and assign remaining text to a new column
WCFP_BGCI_data_all3 <- WCFP_BGCI_data_all3 %>%
  mutate(
    # Keep original name for splitting
    taxon_and_authors_standardized_WFO = taxon_name_standardized_WFO,
    # Extract the first two words (Genus & Species)
    taxon_name_standardized_WFO = word(taxon_and_authors_standardized_WFO, 1, 2),
    # Extract the remaining words as authors
    taxon_authors_standardized_WFO = str_trim(str_remove(taxon_and_authors_standardized_WFO, paste0("^", word(taxon_and_authors_standardized_WFO, 1, 2), "\\s*")))
     )


## Filter for our crops/list
# read in plant list 
WCFP_plantlist <- read_excel("C:/Users/sarah/Desktop/Agrobiodiversity/GCCFP/Plants_list/WCFP_simplified_240921.xlsx")
# filter for WCFP crops 
# (1) Keep rows in WCRP_BGCI_data_all3 where the taxa name (taxon_name_accepted) 
# in WCFP_plantlist matches the standardized taxa names (taxon_name_standardized_WFO) in WCRP_BGCI_data_all3
# (2) if there is an NA in the taxon_name_standardized_WFO then keep rows if taxa name (taxon_name_accepted) 
# in WCFP_plantlist matches the taxon name submitted (taxon_name_standardized_WFO) in WCRP_BGCI_data_all3
# (3) or if NA in standardized name then if WCFP_plantlist matches the taxon synonym (taxon_synonymous_name_PlantSearch) in WCRP_BGCI_data_all3
# (4) or keep rows directly where taxon_name_submitted matches taxon_name_accepted, even if taxon_name_standardized_WFO is not NA.

WCFP_BGCI_data_filtered <- WCFP_BGCI_data_all3 %>%
  filter(
    taxon_name_standardized_WFO %in% WCFP_plantlist$taxon_name_accepted |
      (is.na(taxon_name_standardized_WFO) & taxon_name_submitted %in% WCFP_plantlist$taxon_name_accepted) |
      (is.na(taxon_name_standardized_WFO) & taxon_synonymous_name_PlantSearch %in% WCFP_plantlist$taxon_name_accepted) |
      taxon_name_submitted %in% WCFP_plantlist$taxon_name_accepted
  )

# View the dropped rows:
# View the filtered-out rows, 72 taxa 
# Still need to hand check these
filtered_out_rows <- anti_join(WCFP_BGCI_data_all3, WCFP_BGCI_data_filtered, by = colnames(WCFP_BGCI_data_all3))
View(filtered_out_rows)







## Combine BGCI plant search all data and genesys all data into one datatset?

# WCFP_Genesys_data_all3 - 4,863,459 entries, 19 total columns
# WCFP_BGCI_data_all3 - 621,651 entries, 23 total columns



## this is a lot of data to merge efficiently, so filter by our plant names first on the ind datasets


# remove duplicates 


              
              
              
              
              
              
              

### to do: Annotate the Plant List
# read in plant list 
WCFP_plantlist <- read_excel("C:/Users/sarah/Desktop/Agrobiodiversity/GCCFP/Plants_list/WCFP_simplified_240921.xlsx")


##### 1. major crops vs minor crops
## anything NOT on ptftw list as minor crop
##### 2. Cultivated vs wild? - how to easily do?
##### 3. Food group (cereal, pulse, vegetable, etc.)? 
  


