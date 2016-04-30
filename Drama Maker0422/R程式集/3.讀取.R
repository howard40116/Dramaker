####讀取用
my.read.csv=function(y)
{
read.csv(paste0("C:/Users/Student/Desktop/data/",y,".CSV"))
}

x1=my.read.csv("127 HOURS")
x2=my.read.csv("avatar")
x3=my.read.csv("avengers")
x4=my.read.csv("bigeyes")
x5=my.read.csv("croods")
x6=my.read.csv("dragon")
x7=my.read.csv("exmachina")
x8=my.read.csv("Interstellar")
x9=my.read.csv("moneyball")
x10=my.read.csv("revenant")
x11=my.read.csv("TheKeepingRoom")
x12=my.read.csv("TMNT")
x13=my.read.csv("Transformers")
x14=my.read.csv("DespicableMe2")
x15=my.read.csv("FaultInOurStars")
x16=my.read.csv("FROZEN")
x17=my.read.csv("HappyFeet")
x18=my.read.csv("SocialNetwork")
x19=my.read.csv("StarWarsTheForceAwakens")


head(x1)


####整理用

library(dplyr)

my.read=function(x)
{
x.filter=filter(x, Freq > 10 )
x.filter.sort = x.filter[order(x.filter$Freq) , ]
x.filter.sort
}



nrow(x1.filter)   #檢查有幾列

x1filtersort=my.read(x1)
x2filtersort=my.read(x2)
x3filtersort=my.read(x3)
x4filtersort=my.read(x4) #
x5filtersort=my.read(x5) #
x6filtersort=my.read(x6)
x7filtersort=my.read(x7) #
x8filtersort=my.read(x8)
x9filtersort=my.read(x9)
x10filtersort=my.read(x10)
x11filtersort=my.read(x11)
x12filtersort=my.read(x12)
x13filtersort=my.read(x13)
x14filtersort=my.read(x14)
x15filtersort=my.read(x15)
x16filtersort=my.read(x16) #
x17filtersort=my.read(x17)
x18filtersort=my.read(x18)
x19filtersort=my.read(x19)



##
write.table(x1filtersort,paste0("C:/Users/howard/Desktop/new/x1filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x1filtersort,paste0("C:/Users/howard/Desktop/new/x1filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x2filtersort,paste0("C:/Users/howard/Desktop/new/x2filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x3filtersort,paste0("C:/Users/howard/Desktop/new/x3filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x4filtersort,paste0("C:/Users/howard/Desktop/new/x4filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x5filtersort,paste0("C:/Users/howard/Desktop/new/x5filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x6filtersort,paste0("C:/Users/howard/Desktop/new/x6filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x7filtersort,paste0("C:/Users/howard/Desktop/new/x7filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x8filtersort,paste0("C:/Users/howard/Desktop/new/x8filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x9filtersort,paste0("C:/Users/howard/Desktop/new/x9filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x10filtersort,paste0("C:/Users/howard/Desktop/new/x10filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x11filtersort,paste0("C:/Users/howard/Desktop/new/x11filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x12filtersort,paste0("C:/Users/howard/Desktop/new/x12filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x13filtersort,paste0("C:/Users/howard/Desktop/new/x13filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x14filtersort,paste0("C:/Users/howard/Desktop/new/x14filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x15filtersort,paste0("C:/Users/howard/Desktop/new/x15filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x16filtersort,paste0("C:/Users/howard/Desktop/new/x16filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x17filtersort,paste0("C:/Users/howard/Desktop/new/x17filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x18filtersort,paste0("C:/Users/howard/Desktop/new/x18filtersort.CSV"), sep = "," , row.names = FALSE)
write.table(x19filtersort,paste0("C:/Users/howard/Desktop/new/x19filtersort.CSV"), sep = "," , row.names = FALSE)






####合併readcsv用
my.read.csv=function(y)
{
read.csv(paste0("C:/Users/Student/Desktop/data/",y,".CSV"))
}
x1=read.csv("C:/Users/Student/Desktop/readuse.csv")
x2=paste0(x1$x1,x1$x2,x1$x3)
write.csv(x2,"C:/Users/Student/Desktop/readuse2.csv")












