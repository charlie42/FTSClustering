#rm(list=ls())

Needed <- c("arules", "SnowballC", "tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc", "stringr")   

#install.packages(Needed, dependencies=TRUE)   

cname <- file.path("~", "Desktop", "abstracts")   
dir(cname)

library(tm)  
library(SnowballC) 
library(stringr)

docs <- Corpus(DirSource(cname))   

replacePunctuation <- content_transformer(function(x) {return (gsub("[[:punct:]]"," ", x))})
docs <- tm_map(docs, replacePunctuation)  
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)  

dtm <- DocumentTermMatrix(docs)  
tdm <- TermDocumentMatrix(docs)  

DF <- data.frame(document=rep(NA, nrow(dtm)), terms=rep(NA, nrow(dtm)),  # as many cols as you need
                 stringsAsFactors=FALSE)          # you don't know levels yet

max <- nrow(dtm)
for (i in 1:max) {
  dtm <- DocumentTermMatrix(docs[i])
  terms <- colnames(dtm)
  DF$document[i] <- rownames(dtm)
  DF$terms[i] <- list(terms)
}
row.names(DF)=DF$document

library(arules)
t <- as(DF$terms, "transactions")
rules <- eclat(t, parameter = list(supp = 0.7))
rulesDF <- as(rules, "data.frame") 

FTS_document_matrix <- matrix(nrow=length(rules), ncol=length(DF$document))
colnames(FTS_document_matrix) <- DF$document
rownames(FTS_document_matrix) <- levels(rulesDF$items)

for(doc_num in 1:length(DF$document)) {
  fts_row_counter <- 0
  for(set in rownames(FTS_document_matrix)) {
    fts_row_counter <- fts_row_counter + 1
    term_counter <- 0
    terms <- strsplit(set, ",")
    for(term in terms[[1]]) {
      term <- str_replace_all(term, "[^[:alnum:]]", "")
      #browser()
      if(term %in% DF[doc_num,2][[1]]) {
        term_counter <- term_counter + 1
        #browser()
      }
     if(term_counter == length(terms)){
       FTS_document_matrix[fts_row_counter, doc_num] <- 1
       term_counter <- 0
     }
    }
  }
}

FTS_document_matrix[is.na(FTS_document_matrix)] <- 0

calculate_entropy <- function() {
  entropy_overlap = 0
  for(i in 1:nrow(FTS_document_matrix)) {
    entropy_overlap[i] <- 0
    for(j in 1:ncol(transposed_entropy)) {
      entropy_overlap[i] <- entropy_overlap[i] + (transposed_entropy[1,j] * FTS_document_matrix[i,j])
    }
  }
  return(entropy_overlap)
}

clusters = list()

for(cluster in 1:ncol(FTS_document_matrix)) {
  
  fj <- colSums(FTS_document_matrix)
  cover <- rowSums(FTS_document_matrix)
  entropy <- (-(1/fj)*log(1/fj))
  transposed_entropy <- t(entropy)
  
  entropy_overlap <- calculate_entropy()
  
  FTS_document_matrix <- FTS_document_matrix[rowSums(FTS_document_matrix) > 0,]
  
  clusters[[length(clusters)+1]] <-
      names(which(FTS_document_matrix[which.min(entropy_overlap),] != 0))
  docs_to_remove <- which (FTS_document_matrix[which.min(entropy_overlap),] != 0)
  
  FTS_document_matrix[min(entropy_overlap), FTS_document_matrix[min(entropy_overlap),] != 0] <- 0
  FTS_document_matrix<-FTS_document_matrix[-(which.min(entropy_overlap)),]
  FTS_document_matrix<-FTS_document_matrix[,-docs_to_remove]
}

clusters




