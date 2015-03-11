############################################
######## Lesson 3: Content Insights ########
############################################

######## Exercise 3.1: Understanding source of various tweets ########
library("rworldmap")
geo_data <- as.data.frame(read.xlsx("data.xlsx","Posts by Geography",startRow = 7))
geo_data$Count <- as.numeric(as.character(geo_data$Count))
geo_data_for_map<-joinCountryData2Map(geo_data, joinCode = "NAME", nameJoinColumn = "Location")
mapCountryData(geo_data_for_map, 
               nameColumnToPlot = "Count", 
               mapTitle = "Posts from across the world",
               borderCol = "black",
               oceanCol = "blue",
               missingCountryCol = "white",
               numCats = 100,
               catMethod = "logFixedWidth")

######## Exercise 3.2: Extracting Term Frequency from the tweets ########
library(tm)
english_index<-tweets_data$Language
text <- as.character(tweets_data$Verbatim[english_index])
documents <- data.frame(Text = text)
row.names(documents) <- 1:nrow(documents)
corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
tdm <- TermDocumentMatrix(corpus, list(bounds=list(global=c(5,Inf)),weighting=weightTfIdf,stopwords = TRUE))

######## Exercise 3.3: Identifying top keywords around your brand ########
word_freq<-as.data.frame(apply(tdm,1,mean))
colnames(word_freq)<-c("Frequecy")
word_freq$Words<-row.names(word_freq)
row.names(word_freq) <- 1:nrow(word_freq)
word_freq$Normalized<-100*(word_freq$Frequecy-min(word_freq$Frequecy))/(max(word_freq$Frequecy)-min(word_freq$Frequecy))
wordcloud(word_freq$Words,word_freq$Normalized,scale=c(4,0.5),min.freq=10, max.words=200, random.order=FALSE, rot.per=.5,use.r.layout = TRUE)  

dict<-scan("words.lst", what="character", sep=NULL)
idx<-!is.na(match(word_freq$Words,dict))
word_freq<-word_freq[idx,]
word_freq$Normalized<-100*(word_freq$Frequecy-min(word_freq$Frequecy))/(max(word_freq$Frequecy)-min(word_freq$Frequecy))
wordcloud(word_freq$Words,word_freq$Normalized,scale=c(4,0.05),min.freq=5, max.words=200, random.order=FALSE, rot.per=.5,use.r.layout = TRUE)  

######## Exercise 3.4: Correlation with KPI ########
kpi_list<-as.matrix(as.numeric(as.character(tweets_data$Sentiment)))
word_cloud_kpi<-c()
for (idx in 1:length(word_freq$Words)) {
  cur_word<-word_freq$Words[idx]
  cur_freq<-t(as.matrix(tdm[cur_word,]))
  word_cloud_kpi$word[idx]=cur_word
  word_cloud_kpi$corr[idx]=(cor(cur_freq,kpi_list))
}
word_cloud_kpi$Normalized<-100*(word_cloud_kpi$corr-min(word_cloud_kpi$corr))/(max(word_cloud_kpi$corr)-min(word_cloud_kpi$corr))
wordcloud(word_cloud_kpi$word,word_cloud_kpi$Normalized,scale=c(4,0.5),min.freq=2, max.words=200, random.order=FALSE, rot.per=.5,use.r.layout = TRUE)  

######## Exercise 3.5: Intent in text ########
library("httr")
tweets_data2 = tweets_data[1:1,]
tweets_data2['Verbatim'] = "I don't want to buy a new phone harsh" 
r <- POST("http://52.11.212.179:8080/intent",body=tweets_data2, encode = "json")
stop_for_status(r)
c<-httr::content(r, "parsed", "application/json")
c

#tweets_data3 = tweets_data[1:20,]
#r <- POST("http://52.11.212.179:8080/intent",body=tweets_data3, encode = "json")
#stop_for_status(r)
#c<-httr::content(r, "parsed", "application/json")
#kpi_list<-as.matrix(as.numeric(c))
#word_cloud_kpi<-c()
#idx<-1
#for (idx in 1:length(word_freq$Words)) {
#  cur_word<-word_freq$Words[idx]
#  cur_freq<-t(as.matrix(tdm[cur_word,]))
#  cur_freq[1:20]
#  word_cloud_kpi$word[idx]=cur_word
#  word_cloud_kpi$corr[idx]=(cor(cur_freq[1:20],kpi_list))
#}
#word_cloud_kpi$Normalized<-100*(word_cloud_kpi$corr-min(word_cloud_kpi$corr))/(max(word_cloud_kpi$corr)-min(word_cloud_kpi$corr))
#wordcloud(word_cloud_kpi$word,word_cloud_kpi$Normalized,scale=c(4,0.5),min.freq=20, max.words=200, random.order=FALSE, rot.per=.5,use.r.layout = TRUE)  
