#Function to normalize genus + species; function updated 2026-02
extract_genus_species <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("\\b[×x]\\b", " × ") %>%           # handle × symbol
    str_replace_all("\\+", " + ") %>%                   # handle + sign
    str_replace_all("\\b(sect|subsect|subsp|var|f|subf|aff|cf)\\.", "") %>%  # remove sect. subsect. etc.
    str_squish() %>%
    str_split("\\s+") %>%
    map_chr(~ {
      tokens <- .x[.x != "×" & .x != "+"]              # remove × and + tokens
      if (length(tokens) >= 2) paste(tokens[1:2], collapse = " ") else NA_character_
    })
}