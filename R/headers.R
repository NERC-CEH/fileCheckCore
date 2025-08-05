# # Define function to identify spaces in the headers
# faster version
identify_spaces_in_headers <- function(raw_content_df) {
  # Convert the first row to character
  #headers <- as.character(raw_content_df[1, ])
  headers <- colnames(raw_content_df)
  
  # Find columns where the header contains at least one whitespace character
  has_space <- grepl("\\s", headers)
  
  # Get the indices of these columns
  col_indices <- which(has_space)
  
  # Create cell references using LETTERS for column names and 1 for the row
  cell_refs <- sprintf("%s%s", LETTERS[col_indices], 1)
  
  # Build the output string for each header with spaces
  cell_strings <- sprintf("%s: '%s'", cell_refs, headers[col_indices])
  
  # Combine them into one string separated by commas
  result_string <- paste(cell_strings, collapse = ", ")
  return(result_string)
}

# do this without using the raw_content_df ?  Just use basic read in