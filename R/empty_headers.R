#' Identify empty headers in a data frame
#'
#' This function checks if column names are empty or NA,
#' then checks which columns contain any non-blank or non-NA values.
#' It then determines the indices of columns that have data and an empty top row.
#' It flags and returns a string summarising all problematic cells
#'  with their spreadsheet-style cell references (e.g., A2, B3).
#'
#' @param data A data frame to check
#' @return A single character string listing all flagged cells, or an empty string if none found.
#' @export


empty_headers_fun <- function(data) {
  result <- tryCatch(
    {
      # test if column headers are empty or NA
      empty_name <- is.na(colnames(data)) | trimws(colnames(data)) == ""

      # for each column, check whether it contains any non‐blank, non‐NA values
      has_data <- vapply(data, function(col) {
        any(!is.na(col) & trimws(as.character(col)) != "")
      }, logical(1))

      # find indices that have data and empty header
      cols <- which(empty_name & has_data)

      if (length(cols) == 0) {
        return("")
      }

      col_letters <- sapply(cols, excel_col_letter)
      return(paste(col_letters, collapse = ","))


  },
    error = function(e) {
    return("An error occurred while computing empty_headers")
  })
  return(result)
}




