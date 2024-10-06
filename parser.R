# Install and load required packages
if (!require("mboxr")) install.packages("mboxr")
if (!require("stringr")) install.packages("stringr")
library(mboxr)
library(stringr)

# Function to extract information from email body
extract_info <- function(body) {
  university <- str_extract(body, "(?i)University\\s*(?:of|:)?\\s*[A-Za-z\\s]+")
  course <- str_extract(body, "(?i)Course\\s*(?:Name)?\\s*:?\\s*[A-Za-z0-9\\s]+")
  keyword <- str_extract(body, "(?i)(Economics|Business|Finance|Econometrics|Data Analysis|Statistics)")
  level <- str_extract(body, "(?i)(undergraduate|graduate|UG|GR|BA|MA|MS|PHD|phd|)")
  
  list(
    university = if(!is.na(university)) str_trim(university) else NA,
    course = if(!is.na(course)) str_trim(course) else NA,
    keyword = if(!is.na(keyword)) str_trim(keyword) else NA,
    level = if(!is.na(level)) str_trim(level) else NA
  )
}

# Main function to process MBOX file
process_mbox <- function(mbox_path, output_csv) {
  # Read MBOX file
  mbox <- read_mbox(mbox_path)
  
  # Extract information from each email
  results <- lapply(mbox, function(email) {
    body <- email$body
    info <- extract_info(body)
    c(list(subject = email$subject), info)
  })
  
  # Convert results to data frame
  df <- do.call(rbind, lapply(results, as.data.frame))
  
  # Write results to CSV
  write.csv(df, output_csv, row.names = FALSE)
  
  cat("Processed", nrow(df), "emails. Results saved to", output_csv, "\n")
}

# Usage
output_csv <- "extracted_info.csv"
process_mbox(mbox_path, output_csv)