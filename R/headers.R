#' Identify spaces in the headers of a Data Frame
#'
#' This function scans all column names in a data frame and identifies whitespace characters.
#'  It flags and returns a string of column headers with their spreadsheet-style cell references (e.g. A: Air temp, B: Mass (kg)).
#'
#' @param raw_content_df A data frame containing raw content, from the read_csv_raw function.
#' @return A single character string listing all flagged cells, or an empty string if none found.
#' @export




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
