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