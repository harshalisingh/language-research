library(ggplot2)
library(plyr)
library(stringr)
library(tm)

#Path to folder
filepath <- "~/language-research"

setwd(paste(filepath, "/tech_list", sep = ""))
list <-
  read.csv("lang_list_bigdata.csv",
           header = TRUE,
           stringsAsFactors = TRUE)

industry_list <-
  c("retail", "healthcare", "financial", "telecom", "tech")

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
  mydata.corpus <-
    tm_map(mydata.corpus, removeNonAlphaNumeric, mc.cores = 1)
  
  
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
  
  # convert to Plain Text Document
  mydata.corpus <- tm_map(mydata.corpus, PlainTextDocument)
  
  # build a term-document matrix
  mydata.tdm <- TermDocumentMatrix(mydata.corpus)
  
  #filter terms
  tdm.onlytags <-
    mydata.tdm[rownames(mydata.tdm) %in% list$lower.language,]
  
  #top10
  freq <- sort(rowSums(as.matrix(tdm.onlytags)), decreasing = TRUE)
  sorted.df <- data.frame(lower.language = names(freq), freq = freq)
  
  setwd(paste(filepath, "/plots", sep = ""))
  pdf(file = paste(industry, "_rankings.pdf", sep= ""))
  
  # Bar plot
  q <-
    ggplot(data = sorted.df[sorted.df$freq > 0, ], aes(x = reorder(lower.language, freq), y =
                                                         freq)) +
    geom_bar(stat = "identity",
             width = 0.5,
             fill = "steelblue") +
    geom_text(aes(label=freq), size = 2, hjust= -0.5) +
    ggtitle(toupper(industry)) +
    xlab("") + ylab("Number of Job Postings") +
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
  print(q)
  
  list$industry <-
    sorted.df[match(list$lower.language, sorted.df$lower.language), 2]
  colnames(list)[colnames(list) == 'industry'] <- industry
  
  dev.off()
  
}



list[is.na(list)] <- 0
list$new.total <-
  list$healthcare + list$retail + list$financial + list$telecom + list$tech


pdf(file = "overall_rankings.pdf")
total <-
  ggplot(data = list[list$new.total > 0, ], aes(x = reorder(language, new.total), y =
                                                  new.total)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(aes(label=new.total), size = 2, hjust= -0.5) +
  ggtitle("Overall Ranking") +
  xlab("") + ylab("Number of Job Postings") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

print(total)

dev.off()