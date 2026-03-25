# Correct taxa standardization in WCFP
# reject standardization of names to genus only

#load libraries
library(openxlsx)
library(dplyr)


# Read in WCFP plant list standardized
WCFP_plantlist_stand <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/WCFP_plantlist/Standardized/WCFP_plantlist_standardized_2026-02-23.xlsx")

#view unique genus_species_WFO
unique_genus_species_WFO <- WCFP_plantlist_stand %>%
  distinct(genus_species_WFO)
# View the dataframe
View(unique_genus_species_WFO)

#view unique genus_species
unique_genus_species <- WCFP_plantlist_stand %>%
  distinct(genus_species)
# View the dataframe
View(unique_genus_species)

# Check if there are any genus_species that are the same as genus_species_WFO
matching <- WCFP_plantlist_stand %>%
  filter(genus_species == genus_species_WFO)
# View the result
View(matching)
nrow(matching)

# Remove genus_species if the same as genus_species_WFO
WCFP_plantlist_stand <- WCFP_plantlist_stand %>%
  mutate(genus_species_WFO = ifelse(genus_species == genus_species_WFO, NA, genus_species_WFO))

# Rejected taxa standardization manually
# genus_species
# "amomum roxb."
# "hypertelis e.mey."
# "brassica l."
# "suaeda forssk."
# "mesembryanthemum sect."
# "salvia l."

# Fix "+ pyrocydonia"
WCFP_plantlist_stand <- WCFP_plantlist_stand %>%
  mutate(genus_species = ifelse(genus_species == "+ pyrocydonia", "pyrocydonia danielii", genus_species))

#fix + crataegomespilus
WCFP_plantlist_stand <- WCFP_plantlist_stand %>%
  mutate(genus_species = ifelse(genus_species == "+ crataegomespilus", "crataegomespilus dardari", genus_species))

#save updated WCFP plantlist
write.xlsx(WCFP_plantlist_stand, "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/WCFP_plantlist/Standardized/WCFP_plantlist_standardized_2026-02-23_updated.xlsx")



### end script ###