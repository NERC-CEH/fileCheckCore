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

# # ###  testing - remove later
# file_path <- "C:/Users/matnic/OneDrive - UKCEH/Projects/Automated RACs/test files/test folder/test_header_spaces.csv"
# file_path <- "C:/Users/matnic/OneDrive - UKCEH/Projects/Automated RACs/test files/test folder/data2a.csv"
# data <- read.csv(file_path, stringsAsFactors = FALSE, na.strings = "", check.names = FALSE)
# 
# empty_headers_fun(data)




