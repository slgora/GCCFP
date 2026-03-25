standardize_taxa <- function(df, taxa_col, standardization_table) {
  df %>%
    mutate(
      !!taxa_col := trimws(.data[[taxa_col]]),
      Standardized_taxa = standardization_table[match(.data[[taxa_col]], names(standardization_table))]
    )
}