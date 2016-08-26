library(ggplot2)
library(plyr)
library(stringr)
library(tm)

#Path to folder
filepath <- "~/language-research"
  
# define "tolower error handling" function
try.tolower = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(
    tolower(x),
    error = function(e)
      e
  )
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

setwd(paste(filepath, "/tech_list", sep = ""))
list <- read.csv("lang_list_bigdata.csv", header = TRUE, stringsAsFactors = TRUE)
states <- read.csv("state_name.csv", header = TRUE, stringsAsFactors = TRUE)

loc.df <- data.frame(matrix(ncol = length(states$name) + 1, nrow = length(list$language)))
colnames(loc.df)<- sapply(states$name, try.tolower)
rownames(loc.df)<- sapply(list$language, try.tolower)

loc.df[is.na(loc.df)] <- 0

state_df <- data.frame(code = states$abbr, state = states$name)

states$name <- sapply(states$name, try.tolower)
states$abbr <- sapply(states$abbr, try.tolower)
states$capital <- sapply(states$capital, try.tolower)
states$largest <- sapply(states$largest, try.tolower)

industry_list <-
  c("financial", "tech", "healthcare", "telecom", "retail")

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
  
  df <- data.frame(A = dataset$V1, B = dataset$V2)
  df$D <- paste(df$A, df$B, sep = " ")
  mydata.vectors <- df$D
  
  
  #Change all variation to lower
  list$variation <- sapply(list$variation, try.tolower)
  list$lower.language <- sapply(list$language, try.tolower)
  
  # build a corpus
  mydata.corpus <- Corpus(VectorSource(mydata.vectors))
  
  mydata.corpus <- tm_map(mydata.corpus,
                          content_transformer(function(x)
                            iconv(x, to = 'UTF-8-MAC', sub = 'byte')),
                          mc.cores = 1)
  
  # remove unnecessary spaces
  mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)
  
  # make each letter lowercase
  mydata.corpus <-
    tm_map(mydata.corpus, content_transformer(tolower), mc.cores = 1)
  
  # replace ++ and #
  replacePlus <-
    content_transformer(function(x) {
      return (gsub("++", "pp", x, fixed = TRUE))
    })
  replaceHash <-
    content_transformer(function(x) {
      return (gsub("#", "csharp", x, fixed = TRUE))
    })
  mydata.corpus <- tm_map(mydata.corpus, replacePlus, mc.cores = 1)
  mydata.corpus <- tm_map(mydata.corpus, replaceHash, mc.cores = 1)
  
  # remove non-alphanumeric characters
  removeNonAlphaNumeric <-
    content_transformer(function(x) {
      return (gsub("[^a-zA-Z2-4+#]", " ", x))
    })
  mydata.corpus <- tm_map(mydata.corpus, removeNonAlphaNumeric, mc.cores = 1)
  
  
  # remove generic and custom stopwords
  mydata.corpus <-
    tm_map(mydata.corpus, removeWords, stopwords('english'))
  
  
  # replace all the variations
  for (c in seq(mydata.corpus))
  {
    for (l in 1:length(list$language)) {
      k <- strsplit(list$variation[l], ",")
      if (length(k[[1]] != 0)) {
        for (v in 1:length(k[[1]])) {
          mydata.corpus[[c]] <-
            gsub(paste("\\b", k[[1]][v], "\\b", sep = ""),
                 list$lower.language[l],
                 mydata.corpus[[c]])
        }
      }
    }
  }
  
  # replace all the variations
  for (c in seq(mydata.corpus))
  {
    for (s in 1:length(states$name)) {
      mydata.corpus[[c]] <-
        gsub(paste("\\b", states$capital[s], "\\b", sep = ""), states$name[s], mydata.corpus[[c]])
      mydata.corpus[[c]] <-
        gsub(paste("\\b", states$largest[s], "\\b", sep = ""), states$name[s], mydata.corpus[[c]])
    }
  }
  
  
  # convert to Plain Text Document
  mydata.corpus <- tm_map(mydata.corpus, PlainTextDocument)
  
  for(i in 1:length(mydata.corpus)){
    content <- mydata.corpus[[i]]$content
    for(j in 1:length(list$lower.language)){
      found <- FALSE
      lang <- list$lower.language[j]
      count.lang <- count(grep(paste("\\b", lang, "\\b", sep=""), content))
      if(length(count.lang$x) > 0){
        for(s in 1:50){
          state <- states$name[s]
          if(grepl(state,content)){
            found <- TRUE
            init.count <- loc.df[j,s]
            loc.df[j,s] <- init.count + length(count.lang$x) 
          }
        }
        if(!found){
          init.count <- loc.df[j,51]
          loc.df[j,51] <- init.count + length(count.lang$x) 
        }
      }
      
      }
    }
  
  mydata.tdm <- TermDocumentMatrix(mydata.corpus)
  tdm.onlytags <- mydata.tdm[rownames(mydata.tdm)%in%list$lower.language,]
  
}

#Save Intermediate dataset for further processing
setwd(paste(filepath, "/intermediate", sep = ""))
new_df <- cbind(state_df, t(loc.df[,1:50]))
new_df$total <- rowSums(new_df[,3:44])
write.csv(new_df, file = "industry.df.bigdata.csv", row.names = FALSE)


