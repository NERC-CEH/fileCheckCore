#' Identify empty columns in a Data Frame
#'
#' This function scans all columns in a data frame for the absence of data. It flags and returns a string
#' of empty columns with their spreadsheet-style cell references (e.g. A, B).
#'
#' @param data A data frame to check.
#' @return A single character string listing all flagged columns, or an empty string if none found.
#' @export


empty_columns_fun <- function(data) {
  result <- tryCatch({
    cols <- which(
      vapply(data, function(x) all(is.na(x) | x == ""), logical(1))
    )
    if (length(cols) > 0) {
      col_letters <- sapply(cols, excel_col_letter)
      return(paste(col_letters, collapse = ","))
    } else {
      return("")
    }
  }, error = function(e) {
    return("An error occurred while computing empty_columns")
  })
  return(result)
}

#' Identify empty rows in a Data Frame
#'
#' This function scans all rows in a data frame for the absence of data. It flags and returns a string
#' of empty rows with their spreadsheet-style cell references (e.g. 1, 2).
#'
#' @param data A data frame to check.
#' @return A single character string listing all flagged rows, or an empty string if none found.
#' @export



# Check for empty rows
empty_rows_fun <- function(data) {
  result <- tryCatch({
    if (nrow(data) == 0) {
      return("There are no rows under the first row!")
    }

    empty_rows <- which(
      rowSums(
        do.call(cbind, lapply(data, function(x) is.na(x) | as.character(x) == ""))
      ) == ncol(data)
    ) + 1  # Adjust for header

    if (length(empty_rows) == 0) {
      return("")
    } else {
      return(paste(empty_rows, collapse = ","))
    }
  }, error = function(e) {
    return("An error occurred while computing empty rows")
  })

  return(result)
}
