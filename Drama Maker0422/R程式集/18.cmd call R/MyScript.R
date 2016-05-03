library(tidyr)
library(data.table)
library(magrittr)
library(dplyr)
library(reshape2)
library(e1071)

argv<-commandArgs(TRUE)

#argv <- gsub("\\,",replacement="\\,",argv)

getelementmatrix=function(...)
{
x=c(...)
colmame89=c("animal","army","art","baseball","bear","beer","blue","boss","brother",
"buffalo","camera","castle","cave","cell","cigarette","city","cliff","college",
"colony","company","computer","desert","dorm","dragon","driver","duke","evil",
"family","father","fire","fish","friend","galaxy","gun","helicopter","horse",
"hotel","human","hunter","husband","iron","judge","jungle","keyword","kid",
"knife","laboratory","lie","love","magic","manager","mask","meeting","missal",
"money","monster","mother","mountain","music","news","ninja","office","paint","party",
"plane","planet","president","prince","princess","queen","raven","rain","resistance",
"restaurant","river","robot","science","sea","ship","sister","slave","snow","soldier",
"study","train","village","water","white","wife")

x2<-strsplit(x,split=",",fixed=T)
movie=as.list(colmame89)         #c(col1-col89)


t1=cbind(v1=unlist(x2),v2=1)
t2=cbind(v1=movie,v2=0)
t3=rbind(t1,t2)
dt = data.table(V1 = as.character(t3[,1]) , V2 = as.integer(t3[,2]))
dt2=dt %>% group_by(V1) %>% summarise(Freq=sum(V2))
dt3=dt2[order(dt2$V1,dt2$Freq),]
dt4=t(dt3)
dt5=dt4[2,]
dt6=sub("0",replacement="N",dt5)
dt7=sub("1",replacement="Y",dt6)
dt8=factor(dt7,levels=c("N","Y"))
dt9=t(dt8)
dt10=t(as.data.frame(dt9,rownames=FALSE))
colnames(dt10) <- colmame89  #c(col1-col89)
dt11=data.frame(dt10,row.names=1)

x1=read.csv("C:/Users/Student/Desktop/catetable.csv")
yoyo=read.csv("C:/Users/Student/Desktop/yoyo.csv")
x1=na.exclude(x1)
x1.tree=naiveBayes(x1$imdb ~ . , data=x1)
pred <- predict(x1.tree, x1)
xxxxxx<-rbind(yoyo[5,],dt11)
score<-predict(x1.tree,xxxxxx[2,])
print(as.character(score))
}

#getelementmatrix("water","white")
getelementmatrix(argv)
