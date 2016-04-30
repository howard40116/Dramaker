merge=read.csv(paste0("C:/Users/Student/Desktop/merge.csv"),header=TRUE)
merge=paste0(merge$x1,merge$x2,merge$x3,merge$x4,merge$x5)
write.csv(merge,"C:/Users/Student/Desktop/merge3.CSV", row.names = FALSE)