library(tm)
library(dplyr)

#檔案路徑
path1 <- "C:/Users/howard/Desktop/script/dragon.txt"
#目錄路徑
path2 <- "C:/Users/howard/Desktop/script"
#讀txt檔看資料
tmp1 <- readLines(file(path1,encoding = "UTF-8"))
#讀目錄資訊
src <- DirSource(path2)
#Create volatile corpora 
ovid <- VCorpus(src)
#去除空白
ovid2 <- tm_map(ovid, stripWhitespace)
#轉小寫
ovid3 <- tm_map(ovid2,tolower)
#去除不重要的字元(EX介係詞)
ovid4 <- tm_map(ovid3,removeWords,stopwords("en"))
#去除標點符號
ovid5 <- tm_map(ovid4, removePunctuation)
#去除數字
ovid6 <- tm_map(ovid5, removeNumbers)
##(好像不重要)
##tdm <- TermDocumentMatrix(ovid2)
##inspect(tdm[2,])
#如何看資料
tmp3 <- as.character(ovid6[[1]])
#轉成data.frame格式
mores1 <- data.frame(x = as.character(ovid6[[1]]))
#去除列的NULL
mores2 <- filter(mores1,x != "")
#去除列的空白值
mores3 <- filter(mores2,x != " ")
#將資料前的空白去除
mores4 <- sapply(mores3,"[",1:lengths(mores3)[[1]])
#以字跟字的空白切割
mores5 <- strsplit(mores4," ")
#將個別list內資料抓出來
mores6 <-unlist(mores5)
mores7 <-data.frame(x = mores6) %>% filter(x != "")
mores8 <-table(mores7)
head(mores8 ,n=100)
write.table(mores8,"C:/Users/howard/Desktop/Temp3.CSV", sep = ",")

length(mores8)
plot(mores8)



#  for(i in 1:3786){
#  j=1
#  
#while(j <= length(mores5[[i]]))
#{
#  if (mores5[[i][j] != ""){x1=paste(x1,mores5[[i]][j])}j=j+1}
#}
#}