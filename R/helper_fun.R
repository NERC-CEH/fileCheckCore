#' Convert numbers to excel-style column letter
#'
#' Converts a numeric column index (e.g., 1, 2, ..., 28) into the corresponding Excel-style column
#' letters (e.g., "A", "B", ..., "AB").
#'
#' @param col_index A positive integer representing the column index.
#' @return A character string representing the Excel column letter(s)
#' @keywords internal


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



#' Choose and display a random spinner from the waiter package
#'
#' Helper function to choose a random spinner to show while code is executing. No input arguments.
#'
#' @return Called for its side effect (displays a spinner). Returns `NULL` invisibly.
#' @import waiter
#' @keywords internal

random_spinner <- function() {
  # Get all available spinner function names
  spinner_names <- grep("^spin_", getNamespaceExports("waiter"), value = TRUE)
  # Choose one randomly
  selected_name <- sample(spinner_names, 1)
  # Get the actual function and call it
  get(selected_name, envir = asNamespace("waiter"))()
}
