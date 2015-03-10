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
wordcloud(wc_data$handle[klout_filter], log10(wc_data$Followers[klout_filter]), scale=c(2,0.0001), min.freq=20, max.words=200, random.order=FALSE, rot.per=.5, use.r.layout = TRUE, colors=brewer.pal(9,"PRGn")) 

wordcloud(wc_data$handle[klout_filter], wc_data$Klout[klout_filter], scale=c(2,0.0001), min.freq=20, max.words=200, random.order=FALSE, rot.per=.5, use.r.layout = TRUE, colors=brewer.pal(9,"PRGn"))

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
