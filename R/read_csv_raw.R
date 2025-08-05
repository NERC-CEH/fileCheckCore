#' Read in data from csv safely to detect encoding issues
#'
#' This function optimistically reads in a file assuming utf-8 encoding. Forces any non UTF-8
#' characters into valid UTF-8, then detects any replaces or hex escapes that were made.
#' If made, the file is read in as raw-bytes and the encoding estimated.
#' The BOM is stripped if present. The content is parsed to a data frame and to a single UTF-8 character string.
#'
#' @param file_path The file path to a csv file
#' @return A list with two elements:
#' \describe{
#'   \item{`raw_content`}{A single UTF-8 character string of the sanitized file contents.}
#'   \item{`raw_content_df`}{A data frame parsed from the sanitized content.}
#' }
#' @import data.table readr stringi
#' @export



read_csv_raw <- function(file_path) {

  # 1) Read as UTF-8 (optimistically)
  txt <- read_file(file_path, locale = locale(encoding = "UTF-8"))

  # 2) Force-sanitize into valid UTF-8 with <xx> escapes
  safe_txt <- iconv(txt, from = "UTF-8", to = "UTF-8", sub = "byte")

  # 3) Now detect problems:
  has_replacement  <- grepl("\uFFFD",           safe_txt, fixed = TRUE)
  has_hex_escapes  <- grepl("<[0-9A-Fa-f]{2}>", safe_txt, perl = TRUE)

  if (has_replacement || has_hex_escapes) {
    # 4) Fallback: detect encoding & re-read raw bytes
    raw_bytes <- readBin(file_path, "raw", file.info(file_path)$size)
    enc_info  <- stri_enc_detect(raw_bytes)[[1]][1, ]
    encoding  <- enc_info$Encoding
    message(sprintf("→ Fallback encoding: %s (%.1f%%)", encoding, enc_info$Confidence*100))

    # strip BOM if present
    if (length(raw_bytes) >= 3 &&
        all(raw_bytes[1:3] == as.raw(c(0xEF,0xBB,0xBF)))) {
      raw_bytes <- raw_bytes[-(1:3)]
    }
    txt        <- rawToChar(raw_bytes)
    safe_txt   <- iconv(txt, from = encoding, to = "UTF-8", sub = "byte")
  }

  # 5) Parse with fread on the *sanitized* safe_txt
  df <- fread(input = safe_txt, encoding = "UTF-8",
              data.table = FALSE)

  return(list(raw_content = safe_txt, raw_content_df = df))
}










