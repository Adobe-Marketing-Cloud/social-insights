#####################################################
######## Check status of installed libraries ########
#####################################################
# package_list<-c("xlsx","wordcloud","tm","rworldmap","httr","plyr")
# status<-as.numeric(lapply(package_list, require, character.only=T))

###############################################
######## Setting the working directory ########
###############################################
setwd("C:/Users/jhamtani/Documents/ATL/summit/social-insights")

######## Loading data ########
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
library('xlsx')
tweets_data<-as.data.frame(read.xlsx("data.xlsx","Documents",startRow = 7))