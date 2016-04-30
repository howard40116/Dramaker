##處理TAGS
TAGS=read.csv("C:/Users/Student/Desktop/TAGS.csv",header=TRUE)

library(dplyr)

TAGS=data.frame(TAGS[2:12])
TAGS=unlist(TAGS)[1:1088]
TAGS=data.frame(TAGS)
TAGS=filter(TAGS,TAGS != "")
TAGS=filter(TAGS,TAGS != " ")




## search
library(sqldf)
##f(命名,關鍵字)
x1filtersort
i=1
for(i in 1:299)
{
x1filtersort=sqldf("SELECT * FROM x1filtersort WHERE x1filtersort.mores7 == 'ext'  ;,row.names=TRUE")
i=i+1
}




paste0 (as.character(TAGS[[1]][1])," | ",as.character(TAGS[[1]][2])," | ",as.character(TAGS[[1]][3])," | ",as.character(TAGS[[1]][4]))







## inner join
library(sqldf)

df4 <- sqldf("SELECT * 
              FROM x1filtersort
              JOIN x2filtersort USING(mores7)")
			  
