# Untuk membuat model dan classifier yang akan digunakan di shiny app

# Load library
library(tidyverse)
library(tm)
library(wordcloud)
library(igraph)
library(ggraph)
library(readr)
library(circlize)
library(reshape2)
library(textdata)
library(syuzhet)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)

# Load dataset
setwd("E:/Materi") # set directory sesuai dengan direktori anda
myDataset <- read.csv("data_testing_full.csv", stringsAsFactors = FALSE)

myDataset$length <- str_count(myDataset$content)
length_df <- myDataset %>%
  summarise(length = sum(length))

#dataset disimpan food_review
food_review <- myDataset$content
#Mengubah data reviewnya ke bentuk corpus
docs <- Corpus(VectorSource(food_review))
# membersihkan dataset dari space yang berlebihan
docs <- tm_map(docs, stripWhitespace)
# Mengubah corpus jadi dtm
tdm <- TermDocumentMatrix(docs)
m <- as.matrix(tdm)
# memperoleh jumlah kata dalam urutan menurun
word_freqs = sort(rowSums(m), decreasing=TRUE)
# membuat data frame berdsarkan kata-kata dan frekuensinya
texts_wc_df <- data.frame(word=names(word_freqs), freq=word_freqs)

texts_wc_df <- texts_wc_df[1:300,]

# plotting wordcloud nya

set.seed(1234)
wordcloud(words = texts_wc_df$word, freq = texts_wc_df$freq,
          min.freq = 1,scale=c(1.8,.5),
          max.words=200, random.order=FALSE, rot.per=0.15,colors=brewer.pal(8, 'Dark2'))

# Mendapatkan nilai sentimen
a <-as.character(myDataset$content)
ty_sentiment <- get_nrc_sentiment(a)

# data frame dengan nilai sentimen kumulatif nya
sentimentscores<-data.frame(colSums(ty_sentiment[,]))

# dataframe berdasarkan sentimen dan skor sebagai kolom
names(sentimentscores) <- 'Score'
sentimentscores <- cbind('sentiment'=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

# Plot untuk sentimen kumulatif
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = 'identity')+
  theme(legend.position='none')+
  xlab('sentiments')+ylab('Scores')+
  theme_minimal()
