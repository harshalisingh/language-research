
## Load Data

#Path to folder
filepath <- "~/language-research"

industry_list <-
  c("healthcare", "retail", "financial", "telecom", "tech")

for (industry in industry_list) {
  setwd(paste(filepath, "/data/", industry, sep = ""))
  file_list <- list.files(pattern = "results.csv")
  
  for (file in file_list) {
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")) {
      dataset <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
    }
    
    # if the merged dataset does exist, append to it
    if (exists("dataset")) {
      temp_dataset <-
        read.csv(file, header = FALSE, stringsAsFactors = FALSE)
      dataset <- rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
    
  }
  
}

## Write dataset to file
setwd(paste(filepath, "/intermediate", sep = ""))
write.csv(dataset, file = "dataset.csv")