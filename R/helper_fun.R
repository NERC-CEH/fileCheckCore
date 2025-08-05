#function to convert numbers to excel column letters
excel_col_letter <- function(col_index) {
  letters <- LETTERS
  base <- length(letters)
  result <- ""
  
  while (col_index > 0) {
    remainder <- (col_index - 1) %% base
    result <- paste0(letters[remainder + 1], result)
    col_index <- (col_index - 1) %/% base
  }
  return(result)
}


random_spinner <- function() {
  # Get all available spinner function names
  spinner_names <- grep("^spin_", getNamespaceExports("waiter"), value = TRUE)
  # Choose one randomly
  selected_name <- sample(spinner_names, 1)
  # Get the actual function and call it
  get(selected_name, envir = asNamespace("waiter"))()
}