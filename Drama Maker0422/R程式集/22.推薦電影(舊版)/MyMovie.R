 library(tidyr)
 library(data.table)
 library(dplyr)
 library(reshape2)

argv<-commandArgs(TRUE)

#argv <- gsub("\\,",replacement="\\,",argv)

getelementmatrix=function(...)
{
# x        <-c("animal","army")
# x        <-c("hunter","knife","missal")
x        <-c(...)
x2       <-strsplit(x,split=",",fixed=T)
sumtable <-read.csv("C:/Users/Student/Desktop/summary.csv")
sumtable <-sumtable[order(sumtable$IMDB,decreasing = TRUE),]

if(length(x2)==1){
moviename<-filter(sumtable,sumtable[,unlist(x2)[1]]=="Y")
}else if(length(x2)==2){
moviename<-filter(sumtable,sumtable[,unlist(x2)[1]]=="Y",sumtable[,unlist(x2)[2]]=="Y")
}else if(length(x2)==3){
moviename<-filter(sumtable,sumtable[,unlist(x2)[1]]=="Y",sumtable[,unlist(x2)[2]]=="Y",sumtable[,unlist(x2)[3]]=="Y")
}else if(length(x2)==4){
moviename<-filter(sumtable,sumtable[,unlist(x2)[1]]=="Y",sumtable[,unlist(x2)[2]]=="Y",sumtable[,unlist(x2)[3]]=="Y",sumtable[,unlist(x2)[4]]=="Y")
}else if(length(x2)==5){
  moviename<-filter(sumtable,sumtable[,unlist(x2)[1]]=="Y",sumtable[,unlist(x2)[2]]=="Y",sumtable[,unlist(x2)[3]]=="Y",sumtable[,unlist(x2)[4]]=="Y",sumtable[,unlist(x2)[5]]=="Y")
}else if(length(x2)==6){
  moviename<-filter(sumtable,sumtable[,unlist(x2)[1]]=="Y",sumtable[,unlist(x2)[2]]=="Y",sumtable[,unlist(x2)[3]]=="Y",sumtable[,unlist(x2)[4]]=="Y",sumtable[,unlist(x2)[5]]=="Y",sumtable[,unlist(x2)[6]]=="Y")
}



#如果找不到3
if(as.character(moviename[1:3,1])[1]%>%is.na()){
  moviename<-filter(sumtable,sumtable[,unlist(x2)[1]]=="Y",sumtable[,unlist(x2)[2]]=="Y",sumtable[,unlist(x2)[3]]=="Y")  
}
#如果找不到2
if(as.character(moviename[1:3,1])[1]%>%is.na()){
  moviename<-filter(sumtable,sumtable[,unlist(x2)[1]]=="Y",sumtable[,unlist(x2)[2]]=="Y")  
}
#如果找不到1
if(as.character(moviename[1:3,1])[1]%>%is.na()){
  moviename<-filter(sumtable,sumtable[,unlist(x2)[1]]=="Y")  
}

print(as.character(moviename[1:3,1]))
}

#getelementmatrix("water","white")
getelementmatrix(argv)
