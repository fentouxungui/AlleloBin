# Increase upload limit to 100 MB
options(shiny.maxRequestSize = 100*1024^2)

library(readxl)

# Helper: detect header automatically
detect_header <- function(filepath, ext) {
  if (ext %in% c("csv", "txt")) {
    first_row <- readLines(filepath, n = 1)
    return(any(grepl("[A-Za-z]", unlist(strsplit(first_row, "[,\\t; ]")))))
  } else if (ext %in% c("xls", "xlsx")) {
    return(TRUE)
  }
  return(FALSE)
}

# Unified reader
read_input_file <- function(filepath, header_detected = TRUE) {
  ext <- tools::file_ext(filepath)
  if (ext == "csv") {
    return(read.csv(filepath, header = header_detected))
  } else if (ext == "txt") {
    return(read.table(filepath, header = header_detected, sep = "", fill = TRUE))
  } else if (ext %in% c("xls", "xlsx")) {
    return(readxl::read_excel(filepath, col_names = header_detected))
  } else {
    stop("Unsupported file type")
  }
}
