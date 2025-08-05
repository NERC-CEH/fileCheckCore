# Function to read in csv data as raw character vector and convert to DF
# read_csv_raw <- function(file_path) {
#   
#   # # Read the file into a character vector
#   # raw_content <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
#   # 
#   # # Remove the Byte Order Mark (BOM) character if present
#   # raw_content[1] <- tryCatch({
#   #   sub("\uFEFF", "", raw_content[1])
#   # }, error = function(e) {
#   #   warning("Error removing BOM: ", e)
#   #   return(raw_content)
#   # })
#   # 
#   # # Split lines into fields using commas
#   # content <- strsplit(raw_content, ",")
#   # 
#   # # Convert content into a dataframe
#   # data_raw <- as.data.frame(do.call(rbind, content), stringsAsFactors = FALSE)
#   
#   
#   ### new approach
#   
#   n <- file.info(file_path)$size
#   raw_bytes <- readBin(file_path, what = "raw", n = n)
#   
#   # Strip a UTF-8 BOM if it’s there (EF BB BF)
#   if (length(raw_bytes) >= 3 &&
#       all(raw_bytes[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))) {
#     raw_bytes <- raw_bytes[-(1:3)]
#   }
#   
#   # Optimistically decode as UTF-8
#   text_raw <- rawToChar(raw_bytes)
#   
#   # wrap iconv in tryCatch so we can detect failure
#   raw_content <- tryCatch({
#     txt <- iconv(text_raw, from = "UTF-8", to = "UTF-8", sub = "")
#     if (any(is.na(txt))) stop("NA introduced")
#     txt
#   }, error = function(e) {
#     # 4. Fallback: detect real encoding and re-decode
#     enc_info   <- stri_enc_detect(raw_bytes)[[1]]
#     encoding   <- enc_info[1, "Encoding"]
#     confidence <- enc_info[1, "Confidence"]
#     message(sprintf("  → Detected %s (%.1f%%)", encoding, confidence*100))
#     
#     # transcode from detected into UTF-8
#     txt2 <- iconv(text_raw, from = encoding, to = "UTF-8", sub = "")
#     if (any(is.na(txt2))) {
#       stop("Failed to transcode from ", encoding)
#     }
#     txt2
#   })
#   
#   ### end new approach 
#   
#   raw_content_lines <- strsplit(raw_content, "\r?\n", perl = TRUE)[[1]]
#   
#   # Split lines into fields using commas
#   raw_content_fields <- strsplit(raw_content_lines, ",")
#   
#   # Convert content into a dataframe
#   raw_content_df <- as.data.frame(do.call(rbind, raw_content_fields), stringsAsFactors = FALSE)
#   
#   return(list(raw_content = raw_content, raw_content_df = raw_content_df))
# }






read_csv_raw <- function(file_path) {
  library(data.table); library(readr); library(stringi)
  
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










