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
# x        <-c("hunter","knife","missal","animal","army")
x        <-c(...)
x2       <-strsplit(x,split=",",fixed=T)
sumtable <-read.csv("C:/Users/howard/Desktop/summary.csv")
sumtable <-sumtable[order(sumtable$IMDB,decreasing = TRUE),]


#1.全部符合 A&B&C
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



##2.找不到改取隨機三元素
#2.1元素隨機排列
x2.random<-gtools::permute(unlist(x2))

#2.2檢查moviename是否為空的
movienameisna<-moviename[1:3,1][1]%>%as.character%>%is.na()

#2.3開始檢查
if(length(x2)==1 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y")
}else if(length(x2)==2 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y" | sumtable[,x2.random[2]]=="Y")
}else if(length(x2)==3 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y" & sumtable[,x2.random[2]]=="Y")
}else if(length(x2)==4 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y" & sumtable[,x2.random[2]]=="Y" & sumtable[,x2.random[3]]=="Y")
}else if(length(x2)==5 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y" & sumtable[,x2.random[2]]=="Y" & sumtable[,x2.random[3]]=="Y")
}else if(length(x2)==6 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y" & sumtable[,x2.random[2]]=="Y" & sumtable[,x2.random[3]]=="Y")
}


##3.以防萬一A|B|C
#3.1檢查moviename是否為空的
movienameisna<-moviename[1:3,1][1]%>%as.character%>%is.na()
#3.2開始檢查
if(length(x2)==1 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y")
}else if(length(x2)==2 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y" | sumtable[,x2.random[2]]=="Y")
}else if(length(x2)==3 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y" | sumtable[,x2.random[2]]=="Y")
}else if(length(x2)==4 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y" | sumtable[,x2.random[2]]=="Y" | sumtable[,x2.random[3]]=="Y")
}else if(length(x2)==5 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y" | sumtable[,x2.random[2]]=="Y" | sumtable[,x2.random[3]]=="Y")
}else if(length(x2)==6 && movienameisna){
  moviename<-filter(sumtable,sumtable[,x2.random[1]]=="Y" | sumtable[,x2.random[2]]=="Y" | sumtable[,x2.random[3]]=="Y")
}


print(as.character(moviename[1:3,1]))

}


# getelementmatrix("hunter","knife","missal","animal","army")
getelementmatrix(argv)