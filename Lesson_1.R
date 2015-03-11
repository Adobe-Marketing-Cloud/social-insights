#####################################################
######## Check status of installed libraries ########
#####################################################
# package_list<-c("xlsx","wordcloud","tm","rworldmap","httr","plyr")
# install.packages(package_list)
# status<-as.numeric(lapply(package_list, require, character.only=T))

################################################
######## Setting the working directory #########
################################################
setwd("C:/L720")

######## Loading data ########
# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
library('xlsx')
tweets_data<-as.data.frame(read.xlsx("data.xlsx","Documents",startRow = 7))
