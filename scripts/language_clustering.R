library(ggplot2)
library(plyr)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(tm)
library(ggdendro)
library(dendextend)
library(ape)

library(rjson)
library(networkD3)

library(fpc)
library(cluster)


#Path to folder
filepath <- "~/language-research"

setwd(paste(filepath, "/tech_list", sep = ""))
list <-read.csv("lang_list_javascript.csv", header=TRUE, stringsAsFactors = TRUE)

source("data_merge.R")

setwd(paste(filepath, "/intermediate", sep = ""))
dataset <- read.csv(file = "dataset.csv")
df <- data.frame(A = dataset$V1, B = dataset$V2)
df$D <- paste(df$A, df$B, sep=" ")
mydata.vectors <-df$D


# define "tolower error handling" function 
try.tolower = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

#Change all variation to lower
list$variation <- sapply(list$variation, try.tolower)
list$lower.language <- sapply(list$language, try.tolower)

lang_list$variation <- sapply(lang_list$variation, try.tolower)
lang_list$lower.language <- sapply(lang_list$language, try.tolower)

# build a corpus
mydata.corpus <- Corpus(VectorSource(mydata.vectors))

mydata.corpus <- tm_map(mydata.corpus,
                        content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                        mc.cores=1)

# remove unnecessary spaces
mydata.corpus<-tm_map(mydata.corpus,stripWhitespace)

# make each letter lowercase
mydata.corpus <- tm_map(mydata.corpus, content_transformer(tolower), mc.cores=1)

# remove non-alphanumeric characters
removeNonAlphaNumeric <- content_transformer(function(x) {return (gsub("[^a-z2-4+#.]"," ",x))})
mydata.corpus <- tm_map(mydata.corpus, removeNonAlphaNumeric, mc.cores=1)

# replace ++ and #
replacePlus <- content_transformer(function(x) {return (gsub("++", "pp", x, fixed = TRUE))})
replaceHash <- content_transformer(function(x) {return (gsub("#", "csharp", x, fixed = TRUE))})
mydata.corpus <- tm_map(mydata.corpus, replacePlus, mc.cores = 1)
#mydata.corpus <- tm_map(mydata.corpus, replaceHash, mc.cores = 1)

# replace punctuation 
replacePunctuation <- function(x) {return (gsub("[[:punct:]]"," ", x))}
mydata.corpus <- tm_map(mydata.corpus, content_transformer(replacePunctuation), mc.cores=1)




# remove generic and custom stopwords
mydata.corpus <- tm_map(mydata.corpus, removeWords, stopwords('english'))



# replace all the variations
for (c in seq(mydata.corpus))
{
  for (l in 1:length(list$language)){
    k <- strsplit(list$variation[l], ",")
    if(length(k[[1]] != 0)){
      for(v in 1:length(k[[1]])){
        mydata.corpus[[c]] <- gsub(paste("\\b", k[[1]][v], "\\b", sep=""), list$lower.language[l], mydata.corpus[[c]])
      }
    }
  }
}

# convert to Plain Text Document
mydata.corpus <- tm_map(mydata.corpus, PlainTextDocument)

# build a term-document matrix
mydata.tdm <- TermDocumentMatrix(mydata.corpus)

tdm.onlytags <- mydata.tdm[rownames(mydata.tdm)%in%list$lower.language,]
tdm.smalltags <- mydata.tdm[rownames(mydata.tdm)%in%lang_list$lower.language,]

tdm.onlytags$dimnames$Terms
tdm.smalltags$dimnames$Terms


#Radial Dendo Network
png(file="cloud.png", width=720, height=1000)
my.tdm <- as.matrix(tdm.smalltags)
distMatrix <- dist(scale(my.tdm))
hc <- hclust(distMatrix)
dendroNetwork(hc, treeOrientation = "vertical", height = 500, width = 800, linkColour = "#ccc", linkType = "diagonal")
dev.off()


#K-means
kfit <- kmeans(distMatrix, 10)   
clusplot(as.matrix(distMatrix), kfit$cluster, color=T, shade=T, labels=2, lines=0)   


# Create a complex dend:
dend <- tdm.onlytags %>% scale %>% dist(method = "euclidean") %>% hclust(method = "ward.D2") %>% as.dendrogram %>%
  set("labels_colors") %>% set("labels_cex", c(.9)) 
# plot the dend in usual "base" plotting engine:
plot(dend)

# Create a complex dend:
dend <- tdm.smalltags %>% scale %>% dist(method = "euclidean") %>% hclust(method = "ward.D2") %>% as.dendrogram %>%
  set("labels_colors") %>% set("labels_cex", c(.9)) 
# plot the dend in usual "base" plotting engine:
plot(dend)

# build a document-term matrix
mydata.dtm <- DocumentTermMatrix(mydata.corpus)

# inspect the document-term matrix
mydata.dtm

my.tdm <- as.matrix(tdm.smalltags)
distMatrix <- dist(scale(my.tdm), method = "euclidean")
my.hc <- hclust(distMatrix, method = "ward.D2")
ggdendrogram(my.hc, rotate = TRUE, size = 8, theme_dendro = FALSE)

dev.off()
