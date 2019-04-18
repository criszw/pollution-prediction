library(dplyr)
library(plyr)
time<- read.csv("clean.csv", header=TRUE)
time.s <- tbl_df(time.s)
time.s$Date <- as.Date(time.s$Date, format= c("%m/%d/%Y"))
days <- group_by(time.s, Date)
mean_prec<- c()
a<-aggregate(Precip ~ Date, data=days, mean)
time.s<-cbind( month(a[,1]), a)
colnames(time.s)<-c("month", "date", "precip")
write.csv(time.s, file = "Precip.LeSueur.csv")
}))

library(dplyr)
days %.% group_by(Date) %.% summarise(mean(precip))

library(data.table)
DT <- data.table(days)
DT[, mean(Precip), by = Date]
for(i in 1:nrow(time.s)){
    if(time.s$)
}

time.s$Date <- as.Date(time.s$Date, format= c("%m/%d/%Y"))
head(time.s$Date)
for(i in 1:)
    day1=time.s[which(time.s[,4]=="1/1/(07+)"),]
avgd1=sum(day1[,3])/24