#' Main function to call all csv file checks.
#'
#'
#' @param uploaded_files a data frame of data files to be checked with 2 columns:
#' "name" (containing file name and extension) and "datapath" (containing full file path)
#' @param wacky_chars_check a boolean TRUE/FALSE
#' #' @return A list with two elements:
#' \describe{
#'   \item{`result_df`}{A dataframe of file check results}
#'   \item{`RAC_passed`}{Indicates whether or not all file checks have been passed}
#' }
#' @import fs stringi
#' @export



# # source files in functions folder
# files.sources <- list.files("R/", full.names = TRUE)
# files.sources <- files.sources[!grepl("check_csv_files_export_results.R", files.sources)]
# sapply(files.sources, source)

check_csv_folder_export <- function(uploaded_files,
                                    wacky_chars_check = FALSE
                                    # progress_callback = function(msg) { # function to update progress message
                                    #   cat(msg, "\n") }
) {
  # params:
  # uploaded_files - the list of data files

  if (length(uploaded_files$datapath) == 0) {
    cat("No CSV files uploaded.\n")
    return()
  }

  # Initialize an empty data frame to hold the results
  result_df <- data.frame(
    file_name = character(),
    file_name_check = character(),
    empty_cols = character(),
    empty_rows = character(),
    empty_headers = character(),
    header_spaces = character(),
    flagged_characters = character(),
    stringsAsFactors = FALSE  # Important to prevent factors
  )

  # Preallocate a list to store the result for each file.
  result_list <- vector("list", length(uploaded_files$datapath))


  # loop through all files for name check only, only csv files for rest of functions

  for (i in seq_along(uploaded_files$datapath)) {
    file_path <- uploaded_files$datapath[i]
    file_name <- uploaded_files$name[i]

    cat("Checking file:", file_name, "\n")
    # progress_callback(paste("Checking file:", file_name))

    # check file names for spaces
    #file_name_spaces <- if (grepl("\\s", file_name)) TRUE
    file_name_spaces <- if (grepl("\\s", file_name)) "spaces" else ""

    if (endsWith(file_name, ".csv")) {

      # Read the CSV file
      data <- tryCatch({
        read.csv(file_path, stringsAsFactors = FALSE, na.strings = "", check.names = FALSE)
      },

      error = function(e) {
        error_message <- "Failed to read csv file. Try opening manually"

        # check if its actually an xlsx file that's been renamed as a csv file
        header <- readBin(file_path, "raw", n = 4)
        if (identical(header, as.raw(c(0x50, 0x4B, 0x03, 0x04)))) { # if xlsx file
          error_message <- "Error - This file is likely an Excel XLSX file, not a real CSV even though it has a .csv extension. Please re-save/export it as CSV."
        }

        result_list[[i]] <<- data.frame( # <<- assigns to parent env (outside tryCatch block)
          file_names      = file_name,
          file_name_check = file_name_spaces,
          empty_cols      = error_message,
          empty_rows      = "",
          empty_headers   = "",
          header_spaces   = "",
          flagged_characters   = "",
          stringsAsFactors = FALSE
        )
        return(NULL)
      })

      if (is.null(data)) {
        error_message <- "Error reading file."
        next # Skip to next file if reading failed
      }


      # check file to see if likely a valid csv
      if (nrow(data) == 0 && file.info(file_path)$size > 0) { # if no rows and file isn't empty - suggests not a valid csv
        error_message <- "Found zero rows in a non-empty file — possible corrupt or mislabelled file."

        # check if its actually an xlsx file that's been renamed as a csv file
        header <- readBin(file_path, "raw", n = 4)
        if (identical(header, as.raw(c(0x50, 0x4B, 0x03, 0x04)))) { # if xlsx file
          error_message <- "Error - This file is likely an Excel XLSX file, not a real CSV even though it has a .csv extension. Please re-save/export it as CSV."
        }

        result_list[[i]] <- data.frame(
          file_names      = file_name,
          file_name_check = file_name_spaces,
          empty_cols      = error_message,
          empty_rows      = "",
          empty_headers   = "",
          header_spaces   = "",
          flagged_characters   = "",
          stringsAsFactors = FALSE
        )

      } else { # end of if problem file check

        # Check for empty columns
        empty_columns <- empty_columns_fun(data)

        # Check for empty rows
        empty_rows <- empty_rows_fun(data)

        # check for empty headers
        empty_headers <- empty_headers_fun(data)

        # read in the csv as a raw character vector to deal with characters
        # raw_content_list <- read_csv_raw(file_path) # move to flagged characters function

        # Check for spaces in column headers
        # header_spaces <- identify_spaces_in_headers(raw_content_list$raw_content_df)
        header_spaces <- identify_spaces_in_headers(data) # use simple read in instead

        # Identify non-alphanumeric characters
        if (wacky_chars_check == TRUE) {
          raw_content_list <- read_csv_raw(file_path)
          flagged_characters <- identify_bad_chars_fast(file_path, raw_content_list, regexp_str = allowed_chars)
        } else {
          flagged_characters = "Not checked"
        }

        result_list[[i]] <- data.frame(
          file_names      = file_name,
          file_name_check = file_name_spaces,
          empty_cols      = empty_columns,
          empty_rows      = empty_rows,
          empty_headers   = empty_headers,
          header_spaces   = header_spaces,
          flagged_characters   = flagged_characters,
          stringsAsFactors = FALSE
        )
      } # end if no problems else do all the normal csv check functions

    } else { # end of csv only if conditional


      # fill result table for non-csv files
      result_list[[i]] <- data.frame(
        file_names      = file_name,
        file_name_check = file_name_spaces,
        empty_cols      = "",
        empty_rows      = "",
        empty_headers   = "",
        header_spaces   = "",
        flagged_characters   = "",
        stringsAsFactors = FALSE
      )
    }

  } # end of seq_along(uploaded_files$datapath)) loop

  result_df <- do.call(rbind, result_list)

  RAC_passed <- all(sapply(result_df[, -1], function(col) all(is.na(col) | col == "" | col == "Not checked")))
  print(RAC_passed)

  # Sort dataframe by non-empty columns
  non_empty_cols <- colnames(result_df)[apply(result_df, 2, function(x) any(x != "" & !is.na(x) & !is.null(x)))]
  result_df <- result_df[order(-apply(result_df[non_empty_cols], 1, function(row) sum(!is.na(row) & row != "")), na.last = FALSE), ]

  # Reset row names of the final data frame
  row.names(result_df) <- NULL

  return(list(result_df = result_df, RAC_passed = RAC_passed))
}

