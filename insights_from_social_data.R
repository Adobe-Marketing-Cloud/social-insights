#####################################################
######## Check status of installed libraries ########
#####################################################
# package_list<-c("xlsx","wordcloud","tm","rworldmap","httr","plyr")
# status<-as.numeric(lapply(package_list, require, character.only=T))

###############################################
######## Setting the working directory ########
###############################################
setwd("C:/L720")

######## Loading data ########
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
library('xlsx')
tweets_data<-as.data.frame(read.xlsx("data.xlsx","Documents",startRow = 7))

########################################
######## Lesson2: USER INSIGHTS ########
########################################

######## Exercise 2.2: Filtering Top User ########
library("wordcloud")
wc_data<-c()
unique_authors<-!duplicated(tweets_data$Author_username) 
wc_data$author<-as.character(tweets_data$Author_username[unique_authors])
prefx<-rep("@",length(wc_data$author))
wc_data$handle<-paste(prefx,wc_data$author,sep = '')
wc_data$Klout<-as.numeric(as.character(tweets_data$Klout_score[unique_authors]))
wc_data$Followers<-as.numeric(as.character(tweets_data$Followers[unique_authors]))

klout_filter<-!is.na(wc_data$Klout)
wordcloud(wc_data$handle[klout_filter], wc_data$Klout[klout_filter], scale=c(2,0.0001), min.freq=20, max.words=200, random.order=FALSE, rot.per=.5, use.r.layout = TRUE, colors=brewer.pal(9,"PRGn"))

wordcloud(wc_data$handle[klout_filter], log10(wc_data$Followers[klout_filter]), scale=c(2,0.0001), min.freq=20, max.words=200, random.order=FALSE, rot.per=.5, use.r.layout = TRUE, colors=brewer.pal(9,"PRGn")) 

idx <-order(-wc_data$Followers,-wc_data$Klout)
plot(wc_data$Followers[idx[1:20]],wc_data$Klout[idx[1:20]],log="x",xlab="Number of followers",ylab="Klout", ylim=c(0,100))
text(wc_data$Followers[idx[1:20]],wc_data$Klout[idx[1:20]], labels=wc_data$handle[idx[1:20]], cex= 0.9, pos=3)

######## Exercise 2.3: Brand-Specific User Scoring ########
library("httr")
r <-POST("52.11.212.179:8081/upload", body=tweets_data, encode = "json")
stop_for_status(r)
score <-content(r, "parsed", "application/json")

library("plyr")
score_csv<-ldply(score,data.frame)
write.csv(file="score.csv",score_csv)

score_data = read.csv("score.csv", header = TRUE)
sorted_score_data = score_data[ with(score_data,order(sdScore)) ,  ]
scores = sorted_score_data$sdScore[1:20]
user_ids <- sorted_score_data$user[1:20]
barplot( scores, main="Sentiment score", horiz=TRUE, names.arg=user_ids, las=1, cex.names = 1.0, log = "x" )

sorted_score_data = score_data[with(score_data,order(-cntNeg)),]
scores = sorted_score_data$cntNeg[1:20]
user_ids <- sorted_score_data$user[1:20]
barplot( scores, main="Negative Tweeters", horiz=TRUE, names.arg=user_ids, las=1, cex.names = 1.0 )

sorted_score_data = score_data[with(score_data,order(-sdScore)),]
user_ids <- sorted_score_data$user[1:20]
klout = sorted_score_data$influenceScore[1:20]
adobe = sorted_score_data$sdScore[1:20]
plot(klout, adobe, main="Klout score vs Adobe score", xlab="Klout score ", ylab="Adobe Score ", pch=19)
text(klout, adobe, labels=user_ids, cex= 0.7, pos=3)

content = sorted_score_data$contentScore[1:20]
plot(klout, content, main="Content vs Sentiment", xlab="Content", ylab="Sentiment", pch=19)
text(klout, content, labels=user_ids, cex= 0.7, pos=3)

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
wordcloud(word_cloud_kpi$word,word_cloud_kpi$Normalized,scale=c(4,0.5),min.freq=5, max.words=200, random.order=FALSE, rot.per=.5,use.r.layout = TRUE)  

######## Exercise 3.5: Intent in text ########
r <- POST("http://52.11.212.179:8080/intent",body=tweets_data, encode = "json")
stop_for_status(r)
c<-content(r, "parsed", "application/json")
kpi_list<-as.matrix(as.numeric(c))
word_cloud_kpi<-c()
idx<-1
for (idx in 1:length(word_freq$Words)) {
  cur_word<-word_freq$Words[idx]
  cur_freq<-t(as.matrix(tdm[cur_word,]))
  word_cloud_kpi$word[idx]=cur_word
  word_cloud_kpi$corr[idx]=(cor(cur_freq,kpi_list))
}
word_cloud_kpi$Normalized<-100*(word_cloud_kpi$corr-min(word_cloud_kpi$corr))/(max(word_cloud_kpi$corr)-min(word_cloud_kpi$corr))
wordcloud(word_cloud_kpi$word,word_cloud_kpi$Normalized,scale=c(4,0.5),min.freq=20, max.words=200, random.order=FALSE, rot.per=.5,use.r.layout = TRUE)  
