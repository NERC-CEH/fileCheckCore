
# ^ inside [...] negates the character class, meaning "match anything not in this set."
# allowed_chars <- "[^a-zA-Z0-9\\p{Z}\n\r.,;:'()_/£$%+\\*\\-]"
allowed_chars <- "[^a-zA-Z0-9\\p{Z}\n\r.,;:'()<>_/£$%&+\\*\"\\\\-]"


# Define function to identify non-alphanumeric characters -  add any allowed characters to  allowed_chars
identify_non_alphanumeric_characters_df <- function(raw_content_df, regexp_str = allowed_chars) {
  
  # Convert each column to character, normalize Unicode, and trim spaces in one vectorized pass
  data_cleaned <- as.data.frame(lapply(raw_content_df, function(col) {
    trimws(stri_trans_nfkc(as.character(col)))
  }), stringsAsFactors = FALSE)
  
  # Process each column vectorized and capture problematic cells
  results <- lapply(seq_along(data_cleaned), function(col_idx) {
    col_data <- data_cleaned[[col_idx]]
    # Find indices where cell is non-empty and contains at least one disallowed character
    bad_indices <- which(nchar(col_data) > 0 & grepl(regexp_str, col_data, perl = TRUE))
    
    if (length(bad_indices) > 0) {
      # Create cell references (e.g., A1, B3, etc.) and build the flagged cell string
      cell_refs <- sprintf("%s%s", LETTERS[col_idx], bad_indices + 1)
      sprintf("%s: '%s'", cell_refs, col_data[bad_indices])
    } else {
      NULL
    }
  })
  
  # Collapse all flagged cell strings into one result string
  result_string <- paste(unlist(results), collapse = ", ")
  return(result_string)
}


# scan the entire file as a raw string first and flag characters before checking positions later
identify_bad_chars_fast <- function(file_path, raw_content_list, regexp_str = allowed_chars) {
 
  
   # # Read file as a single string
  # raw_content <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # raw_content <- sub("\uFEFF", "", raw_content, fixed = TRUE)
  
  raw_content <- raw_content_list$raw_content
  

  # Check if any bad characters exist
  if (!grepl(regexp_str, raw_content, perl = TRUE)) {
    # print("No wacky characters found!")
    return("")  # No bad characters found
  } else {
    # print("Wacky characters found so we'll do the slower check")
    
    # Debugging: See what characters are being detected
    bad_chars <- regmatches(raw_content, gregexpr(regexp_str, raw_content, perl = TRUE))
    # print("Characters triggering detection:")
    # print(unique(unlist(bad_chars)))
    
    # If bad characters exist, fall back to slower cell-by-cell checking
    raw_content_df <- raw_content_list$raw_content_df
    
    return(identify_non_alphanumeric_characters_df(raw_content_df, regexp_str = allowed_chars))
  }
}
