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
