#直播收視率
library(dplyr)
library(magrittr)
library(httr)
res = httr::POST(
  url = "http://54.92.30.60:80/api/tv/progtop",
  body = list(
    st="20150501",
    et="20150531",
    t="2",
    d="1",
    token = "api_doc_token"), 
  encode = "form")
res %>% content %>% str
tmp=res %>% content
tmp2=tmp$data
tmp3=sapply(tmp2,"[",1)
write.table(tmp3,"C:/Users/Student/Desktop/Temp.CSV", sep = ",")

#隨選TOP50
library(dplyr)
library(magrittr)
library(httr)
res = httr::POST(
url = "http://54.92.30.60:80/api/tv/vodtop",
body = list(
st="20150501",
et="20151031",
i="0",
g="動作|動作冒險|冒險|兒童|喜劇|遊戲|紀實類記錄片|歷史文化類記錄片|記錄|劇情|數位學習|家庭|恐佈|幼幼|音樂|浪漫愛情|科幻小說|科幻|運動|懸疑|懸疑驚悚|驚悚|戰爭",
t="2",
d="2",
token = "api_doc_token"), 
encode = "form")
res %>% content %>% str
tmp=res %>% content
tmp2=tmp$data
tmp3=sapply(tmp2,"[",2)
tmp4=t(tmp3)
tmp5=cbind(tmp4[1:50])
write.table(tmp5,"C:/Users/Student/Desktop/Temp2.CSV", sep = ",")

##隨選視訊節目資訊
library(dplyr)
library(magrittr)
library(httr)
res = httr::POST(
url = "http://54.92.30.60:80/api/tv/vodsearch",
body = list(
g="兒童",
token = "api_doc_token"), 
encode = "form")
res %>% content %>% str
tmp=res %>% content
tmp2=tmp$data
tmp3=sapply(tmp2,"[",2)
tmp4=t(tmp3)
tmp5=cbind(tmp4[1:ncol(tmp4)])
tmp6=cbind("兒童",tmp5)
write.table(tmp6,"C:/Users/howard/Desktop/Temp4.CSV", sep = ",",row.names=FALSE)

#直播各節目收視率

library(dplyr)
library(magrittr)
library(httr)

search=function(xxx,yyy)
{
res = httr::POST(
url = "http://54.92.30.60:80/api/tv/progwatch",
body = list(
st="20150501",
et="20150531",
t=1,
p=xxx,
d=2,
token = "api_doc_token"), 
encode = "form")
res %>% content %>% str
tmp=res %>% content
tmp2=tmp$data
tmp3=sapply(tmp2,"[",8)
tmp4=t(tmp3)
tmp5=cbind(tmp4[1:ncol(tmp4)])
tmp6=cbind(xxx,tmp5)
write.table(tmp6,paste0("C:/Users/howard/Desktop/data/",yyy,".CSV"), sep = ",",row.names=FALSE)
}

x1=read.csv("C:/Users/howard/Desktop/所有隨選節目.csv")
for(i in 1:1000)
{
x2=x1[[2]][i]
search(x2,i)
}